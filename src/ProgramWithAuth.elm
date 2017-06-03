module ProgramWithAuth exposing (AuthProgram, Config, Model, Msg, program)

import Html exposing (Html)
import Navigation exposing (Location)


{-| `update` and `init` return a `Result` where:

  - `Ok` means auth is finished; the provided value will be used to initialize the main program
  - `Err` means auth did not finish; the provided value is the new model and Cmd for the auth program

-}
type alias AuthProgram flags auth model msg =
    ProgramRecord flags auth model msg


type alias Config flags model msg =
    ProgramRecord flags Never model msg


type alias ProgramRecord flags done model msg =
    { onLocationChange : Maybe (Location -> msg)
    , init : flags -> Location -> Result ( model, Cmd msg ) done
    , update : msg -> model -> Result ( model, Cmd msg ) done
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type Model flags authModel model msg
    = Unauthed flags Location authModel
    | Authed (Config flags model msg) model


type Msg authMsg msg
    = LocationChange Location
    | AuthMsg authMsg
    | MainMsg msg


program :
    AuthProgram flags auth authModel authMsg
    -> (auth -> Config flags model msg)
    -> Program flags (Model flags authModel model msg) (Msg authMsg msg)
program auth ready =
    Navigation.programWithFlags
        LocationChange
        { init = init auth ready
        , update = update auth ready
        , subscriptions = subscriptions auth
        , view = view auth
        }


init :
    AuthProgram flags auth authModel authMsg
    -> (auth -> Config flags model msg)
    -> flags
    -> Location
    -> ( Model flags authModel model msg, Cmd (Msg authMsg msg) )
init auth ready flags location =
    auth.init flags location
        |> handleAuthResult ready flags location


handleAuthResult :
    (auth -> Config flags model msg)
    -> flags
    -> Location
    -> Result ( authModel, Cmd authMsg ) auth
    -> ( Model flags authModel model msg, Cmd (Msg authMsg msg) )
handleAuthResult ready flags location result =
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
                    config.init flags location
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
    AuthProgram flags auth authModel authMsg
    -> (auth -> Config flags model msg)
    -> Msg authMsg msg
    -> Model flags authModel model msg
    -> ( Model flags authModel model msg, Cmd (Msg authMsg msg) )
update auth ready msg model =
    case model of
        Unauthed flags location authModel ->
            case msg of
                LocationChange newLocation ->
                    case auth.onLocationChange of
                        Nothing ->
                            ( model, Cmd.none )

                        Just onLocationChange ->
                            auth.update (onLocationChange newLocation) authModel
                                |> handleAuthResult ready flags newLocation

                AuthMsg authMsg ->
                    auth.update authMsg authModel
                        |> handleAuthResult ready flags location

                MainMsg mainMsg ->
                    Debug.crash "ProgramWithAuth.update: got a MainMsg before auth finished. (This should not be possible.)" ( msg, model )

        Authed config mainModel ->
            case msg of
                LocationChange location ->
                    config.onLocationChange
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


subscriptions auth model =
    case model of
        Unauthed _ _ authModel ->
            auth.subscriptions authModel
                |> Sub.map AuthMsg

        Authed config mainModel ->
            config.subscriptions mainModel
                |> Sub.map MainMsg


view auth model =
    case model of
        Unauthed _ _ authModel ->
            auth.view authModel
                |> Html.map AuthMsg

        Authed config mainModel ->
            config.view mainModel
                |> Html.map MainMsg
