module ProgramWithAuth exposing
    ( AuthProgram
    , Config
    , Model
    , Msg
    , authProgram
    )

import Html exposing (Html)
import Navigation exposing (Location)
import ProgramRecord exposing (ProgramRecord)
import Task exposing (Task)


(>>>) : (a -> b -> y) -> (y -> z) -> (a -> b -> z)
(>>>) y f a b =
    f (y a b)


type alias AuthProgram flags auth model msg =
    ProgramRecord flags auth model msg


type alias Config flags model msg =
    ProgramRecord flags Never model msg


type Model flags authModel model msg
    = Loading (Maybe flags) Location
    | Unauthed (Maybe flags) Location authModel
    | Authed (Config flags model msg) model


type Msg auth authMsg msg
    = LocationChange Location
    | AuthMsg authMsg
    | MainMsg msg
    | IgnoreMsg
    | ReadAuth (Result () auth)


type alias AuthConfig auth =
    { read : Task () auth
    , write : auth -> Task Never ()
    }


authProgram :
    AuthConfig auth
    -> AuthProgram Never auth authModel authMsg
    -> (auth -> Config Never model msg)
    -> ProgramRecord Never Never (Model Never authModel model msg) (Msg auth authMsg msg)
authProgram config auth ready =
    ProgramRecord.navigationProgram
        LocationChange
        { init = init config auth ready Nothing
        , update = update config auth ready
        , subscriptions = subscriptions auth
        , view = view auth
        }


init :
    AuthConfig auth
    -> AuthProgram flags auth authModel authMsg
    -> (auth -> Config flags model msg)
    -> Maybe flags
    -> Location
    -> ( Model flags authModel model msg, Cmd (Msg auth authMsg msg) )
init config auth ready flags location =
    ( Loading flags location
    , Task.attempt ReadAuth config.read
    )


handleAuthResult :
    AuthConfig auth
    -> (auth -> Config flags model msg)
    -> Maybe flags
    -> Location
    -> Result ( authModel, Cmd authMsg ) auth
    -> ( Model flags authModel model msg, Cmd (Msg auth authMsg msg) )
handleAuthResult conf ready flags location result =
    case result of
        Err ( authModel, authCmd ) ->
            ( Unauthed flags location authModel
            , authCmd
                |> Cmd.map AuthMsg
            )

        Ok auth ->
            let
                config =
                    ready auth

                ( mainModel, cmd ) =
                    ProgramRecord.applyInit config.init flags location
                        |> handleNever
            in
            ( Authed config mainModel
            , Cmd.batch
                [ Cmd.map MainMsg cmd
                , Task.perform (\() -> IgnoreMsg) (conf.write auth)
                ]
            )


handleNever : Result a Never -> a
handleNever r =
    case r of
        Ok a ->
            never a

        Err x ->
            x


update :
    AuthConfig auth
    -> AuthProgram flags auth authModel authMsg
    -> (auth -> Config flags model msg)
    -> Msg auth authMsg msg
    -> Model flags authModel model msg
    -> ( Model flags authModel model msg, Cmd (Msg auth authMsg msg) )
update conf auth ready msg model =
    case model of
        Loading flags location ->
            case msg of
                ReadAuth (Ok auth) ->
                    Ok auth
                        |> handleAuthResult conf ready flags location

                ReadAuth (Err ()) ->
                    ProgramRecord.applyInit auth.init flags location
                        |> handleAuthResult conf ready flags location

                _ ->
                    Debug.crash "Got unexpected message while loading"

        Unauthed flags location authModel ->
            case msg of
                LocationChange newLocation ->
                    case ProgramRecord.getLocationChange auth.init of
                        Nothing ->
                            ( model, Cmd.none )

                        Just onLocationChange ->
                            auth.update (onLocationChange newLocation) authModel
                                |> handleAuthResult conf ready flags newLocation

                AuthMsg authMsg ->
                    auth.update authMsg authModel
                        |> handleAuthResult conf ready flags location

                MainMsg mainMsg ->
                    Debug.crash "ProgramWithAuth.update: got a MainMsg before auth finished. (This should not be possible.)" ( msg, model )

                IgnoreMsg ->
                    ( model, Cmd.none )

                ReadAuth auth ->
                    Debug.crash "ProgramWithAuth.update: got ReadAuth after loading finished. (This should not be possible.)" ( msg, model )

        Authed config mainModel ->
            case msg of
                LocationChange location ->
                    ProgramRecord.getLocationChange config.init
                        |> Maybe.map (\f -> f location)
                        |> Maybe.map (\m -> update conf auth ready (MainMsg m) model)
                        |> Maybe.withDefault ( model, Cmd.none )

                AuthMsg authMsg ->
                    Debug.log "ProgramWithAuth.update: got an AuthMsg after auth finished (this could happen if the auth program started a Task or Cmd that did not complete until after the auth finished in some other way)" ( msg, model )
                        |> always ( model, Cmd.none )

                MainMsg mainMsg ->
                    config.update mainMsg mainModel
                        |> handleNever
                        |> Tuple.mapFirst (Authed config)
                        |> Tuple.mapSecond (Cmd.map MainMsg)

                IgnoreMsg ->
                    ( model, Cmd.none )

                ReadAuth auth ->
                    Debug.crash "ProgramWithAuth.update: got ReadAuth after auth finished. (This should not be possible.)" ( msg, model )


subscriptions :
    AuthProgram flags auth authModel authMsg
    -> Model flags authModel model msg
    -> Sub (Msg auth authMsg msg)
subscriptions auth model =
    case model of
        Loading _ _ ->
            Sub.none

        Unauthed _ _ authModel ->
            auth.subscriptions authModel
                |> Sub.map AuthMsg

        Authed config mainModel ->
            config.subscriptions mainModel
                |> Sub.map MainMsg


view :
    AuthProgram flags auth authModel authMsg
    -> Model flags authModel model msg
    -> Html (Msg auth authMsg msg)
view auth model =
    case model of
        Loading _ _ ->
            Html.text ""

        Unauthed _ _ authModel ->
            auth.view authModel
                |> Html.map AuthMsg

        Authed config mainModel ->
            config.view mainModel
                |> Html.map MainMsg
