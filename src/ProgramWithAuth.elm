module ProgramWithAuth
    exposing
        ( AuthProgram
        , Config
        , Model
        , Msg
        , authProgram
        )

import Html exposing (Html)
import Navigation exposing (Location)
import ProgramRecord exposing (ProgramRecord)


(>>>) : (a -> b -> y) -> (y -> z) -> (a -> b -> z)
(>>>) y f a b =
    f (y a b)


type alias AuthProgram flags auth model msg =
    ProgramRecord flags auth model msg


type alias Config flags model msg =
    ProgramRecord flags Never model msg


type Model flags authModel model msg
    = Unauthed Location authModel
    | Authed (Config flags model msg) model


type Msg authMsg msg
    = LocationChange Location
    | AuthMsg authMsg
    | MainMsg msg


authProgram :
    AuthProgram Never auth authModel authMsg
    -> (auth -> Config Never model msg)
    -> ProgramRecord Never Never (Model Never authModel model msg) (Msg authMsg msg)
authProgram auth ready =
    ProgramRecord.navigationProgram
        LocationChange
        { init = init auth ready
        , update = update auth ready
        , subscriptions = subscriptions auth
        , view = view auth
        }


init :
    AuthProgram Never auth authModel authMsg
    -> (auth -> Config Never model msg)
    -> Location
    -> ( Model Never authModel model msg, Cmd (Msg authMsg msg) )
init auth ready location =
    ProgramRecord.applyInit auth.init location
        |> handleAuthResult ready location


handleAuthResult :
    (auth -> Config Never model msg)
    -> Location
    -> Result ( authModel, Cmd authMsg ) auth
    -> ( Model Never authModel model msg, Cmd (Msg authMsg msg) )
handleAuthResult ready location result =
    case result of
        Err ( authModel, authCmd ) ->
            ( Unauthed location authModel
            , authCmd
                |> Cmd.map AuthMsg
            )

        Ok auth ->
            let
                config =
                    ready auth

                ( mainModel, cmd ) =
                    ProgramRecord.applyInit config.init location
                        |> handleNever
            in
            ( Authed config mainModel
            , Cmd.map MainMsg cmd
            )


handleNever : Result a Never -> a
handleNever r =
    case r of
        Ok a ->
            never a

        Err x ->
            x


update :
    AuthProgram Never auth authModel authMsg
    -> (auth -> Config Never model msg)
    -> Msg authMsg msg
    -> Model Never authModel model msg
    -> ( Model Never authModel model msg, Cmd (Msg authMsg msg) )
update auth ready msg model =
    case model of
        Unauthed location authModel ->
            case msg of
                LocationChange newLocation ->
                    case ProgramRecord.getLocationChange auth.init of
                        Nothing ->
                            ( model, Cmd.none )

                        Just onLocationChange ->
                            auth.update (onLocationChange newLocation) authModel
                                |> handleAuthResult ready newLocation

                AuthMsg authMsg ->
                    auth.update authMsg authModel
                        |> handleAuthResult ready location

                MainMsg mainMsg ->
                    Debug.crash "ProgramWithAuth.update: got a MainMsg before auth finished. (This should not be possible.)" ( msg, model )

        Authed config mainModel ->
            case msg of
                LocationChange location ->
                    ProgramRecord.getLocationChange config.init
                        |> Maybe.map (\f -> f location)
                        |> Maybe.map (\m -> update auth ready (MainMsg m) model)
                        |> Maybe.withDefault ( model, Cmd.none )

                AuthMsg authMsg ->
                    Debug.log "ProgramWithAuth.update: got an AuthMsg after auth finished (this could happen if the auth program started a Task or Cmd that did not complete until after the auth finished in some other way)" ( msg, model )
                        |> always ( model, Cmd.none )

                MainMsg mainMsg ->
                    config.update mainMsg mainModel
                        |> handleNever
                        |> Tuple.mapFirst (Authed config)
                        |> Tuple.mapSecond (Cmd.map MainMsg)


subscriptions :
    AuthProgram Never auth authModel authMsg
    -> Model Never authModel model msg
    -> Sub (Msg authMsg msg)
subscriptions auth model =
    case model of
        Unauthed _ authModel ->
            auth.subscriptions authModel
                |> Sub.map AuthMsg

        Authed config mainModel ->
            config.subscriptions mainModel
                |> Sub.map MainMsg


view :
    AuthProgram Never auth authModel authMsg
    -> Model Never authModel model msg
    -> Html (Msg authMsg msg)
view auth model =
    case model of
        Unauthed _ authModel ->
            auth.view authModel
                |> Html.map AuthMsg

        Authed config mainModel ->
            config.view mainModel
                |> Html.map MainMsg
