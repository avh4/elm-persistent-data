module ProgramWithAuth exposing (AuthProgram, Config, Model, Msg, program)

import Html exposing (Html)
import Navigation exposing (Location)


{-| `update` and `init` return a `Result` where:

  - `Ok` means auth is finished; the provided value will be used to initialize the main program
  - `Err` means auth did not finish; the provided value is the new model and Cmd for the auth program

-}
type alias AuthProgram flags auth model msg =
    { onLocationChange : Maybe (Location -> msg)
    , init : flags -> Location -> Result ( model, Cmd msg ) auth
    , update : msg -> model -> Result ( model, Cmd msg ) auth
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type alias Config flags auth model msg =
    { onLocationChange : Maybe (Location -> msg)
    , init : flags -> auth -> Location -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type Model flags authModel model
    = Unauthed flags Location authModel
    | Authed model


type Msg authMsg msg
    = LocationChange Location
    | AuthMsg authMsg
    | MainMsg msg


program :
    AuthProgram flags auth authModel authMsg
    -> Config flags auth model msg
    -> Program flags (Model flags authModel model) (Msg authMsg msg)
program auth config =
    Navigation.programWithFlags
        LocationChange
        { init = init auth config
        , update = update auth config
        , subscriptions = subscriptions auth config
        , view = view auth config
        }


init :
    AuthProgram flags auth authModel authMsg
    -> Config flags auth model msg
    -> flags
    -> Location
    -> ( Model flags authModel model, Cmd (Msg authMsg msg) )
init auth config flags location =
    auth.init flags location
        |> handleAuthResult config flags location


handleAuthResult :
    Config flags auth model msg
    -> flags
    -> Location
    -> Result ( authModel, Cmd authMsg ) auth
    -> ( Model flags authModel model, Cmd (Msg authMsg msg) )
handleAuthResult config flags location result =
    case result of
        Err ( authModel, authCmd ) ->
            ( Unauthed flags location authModel
            , authCmd
                |> Cmd.map AuthMsg
            )

        Ok auth ->
            config.init flags auth location
                |> Tuple.mapFirst Authed
                |> Tuple.mapSecond (Cmd.map MainMsg)


update :
    AuthProgram flags auth authModel authMsg
    -> Config flags auth model msg
    -> Msg authMsg msg
    -> Model flags authModel model
    -> ( Model flags authModel model, Cmd (Msg authMsg msg) )
update auth config msg model =
    case model of
        Unauthed flags location authModel ->
            case msg of
                LocationChange newLocation ->
                    case auth.onLocationChange of
                        Nothing ->
                            ( model, Cmd.none )

                        Just onLocationChange ->
                            auth.update (onLocationChange newLocation) authModel
                                |> handleAuthResult config flags newLocation

                AuthMsg authMsg ->
                    auth.update authMsg authModel
                        |> handleAuthResult config flags location

                MainMsg mainMsg ->
                    Debug.crash "ProgramWithAuth.update: got a MainMsg before auth finished. (This should not be possible.)" ( msg, model )

        Authed mainModel ->
            case msg of
                LocationChange location ->
                    config.onLocationChange
                        |> Maybe.map (\f -> f location)
                        |> Maybe.map (\m -> update auth config (MainMsg m) model)
                        |> Maybe.withDefault ( model, Cmd.none )

                AuthMsg authMsg ->
                    Debug.log "ProgramWithAuth.update: got an AuthMsg after auth finished (this could happen if the auth program started a Task or Cmd that did not complete until after the auth finished in some other way)" ( msg, model )
                        |> always ( model, Cmd.none )

                MainMsg mainMsg ->
                    config.update mainMsg mainModel
                        |> Tuple.mapFirst Authed
                        |> Tuple.mapSecond (Cmd.map MainMsg)


subscriptions auth config model =
    case model of
        Unauthed _ _ authModel ->
            auth.subscriptions authModel
                |> Sub.map AuthMsg

        Authed mainModel ->
            config.subscriptions mainModel
                |> Sub.map MainMsg


view auth config model =
    case model of
        Unauthed _ _ authModel ->
            auth.view authModel
                |> Html.map AuthMsg

        Authed mainModel ->
            config.view mainModel
                |> Html.map MainMsg
