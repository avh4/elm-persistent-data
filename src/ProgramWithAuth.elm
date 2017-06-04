module ProgramWithAuth
    exposing
        ( AuthProgram
        , Config
        , Model
        , Msg
        , ProgramRecord
        , ProgramType(..)
        , authProgram
        , htmlProgram
        , navigationProgram
        , toProgram
        )

import Html exposing (Html)
import Navigation exposing (Location)


{-| `update` and `init` return a `Result` where:

  - `Ok` means auth is finished; the provided value will be used to initialize the main program
  - `Err` means auth did not finish; the provided value is the new model and Cmd for the auth program

-}
type alias ProgramRecord flags done model msg =
    { init : ProgramType flags (Result ( model, Cmd msg ) done) msg
    , update : msg -> model -> Result ( model, Cmd msg ) done
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type ProgramType flags init msg
    = NoArgs init
    | WithFlags (flags -> init)
    | WithLocation (Location -> init) (Location -> msg)
    | WithBoth (flags -> Location -> init) (Location -> msg)


applyInitWithFlags : ProgramType flags init msg -> flags -> Location -> init
applyInitWithFlags init flags location =
    case init of
        NoArgs value ->
            value

        WithFlags f ->
            f flags

        WithLocation f _ ->
            f location

        WithBoth f _ ->
            f flags location


applyInit : ProgramType Never init msg -> Location -> init
applyInit init location =
    case init of
        NoArgs value ->
            value

        WithFlags f ->
            Debug.crash "WithFlags when flags=Never" init

        WithLocation f _ ->
            f location

        WithBoth f _ ->
            Debug.crash "WithBoth when flags=Never" init


getLocationChange : ProgramType flags init msg -> Maybe (Location -> msg)
getLocationChange init =
    case init of
        NoArgs _ ->
            Nothing

        WithFlags _ ->
            Nothing

        WithLocation _ f ->
            Just f

        WithBoth _ f ->
            Just f


toProgram : ProgramRecord Never Never model msg -> Platform.Program Never model msg
toProgram record =
    case record.init of
        NoArgs value ->
            Html.program
                { init = value |> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithFlags init ->
            Html.programWithFlags
                { init = init >> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithLocation init onLocationChange ->
            Navigation.program
                onLocationChange
                { init = init >> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithBoth init onLocationChange ->
            Navigation.programWithFlags
                onLocationChange
                { init = init >>> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }


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


htmlProgram :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> ProgramRecord Never Never model msg
htmlProgram config =
    { init = NoArgs (Err config.init)
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


navigationProgram :
    (Location -> msg)
    ->
        { init : Location -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> ProgramRecord Never Never model msg
navigationProgram onLocationChange config =
    { init = WithLocation (config.init >> Err) onLocationChange
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


authProgram :
    AuthProgram Never auth authModel authMsg
    -> (auth -> Config Never model msg)
    -> ProgramRecord Never Never (Model Never authModel model msg) (Msg authMsg msg)
authProgram auth ready =
    navigationProgram
        LocationChange
        { init = init auth ready
        , update = update auth ready
        , subscriptions = subscriptions auth
        , view = view auth
        }


navigationProgramWithFlags :
    (Location -> msg)
    ->
        { init : flags -> Location -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> ProgramRecord flags Never model msg
navigationProgramWithFlags onLocationChange config =
    { init = WithBoth (config.init >>> Err) onLocationChange
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


init :
    AuthProgram Never auth authModel authMsg
    -> (auth -> Config Never model msg)
    -> Location
    -> ( Model Never authModel model msg, Cmd (Msg authMsg msg) )
init auth ready location =
    applyInit auth.init location
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
                    applyInit config.init location
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
                    case getLocationChange auth.init of
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
                    getLocationChange config.init
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
        Unauthed _ authModel ->
            auth.subscriptions authModel
                |> Sub.map AuthMsg

        Authed config mainModel ->
            config.subscriptions mainModel
                |> Sub.map MainMsg


view auth model =
    case model of
        Unauthed _ authModel ->
            auth.view authModel
                |> Html.map AuthMsg

        Authed config mainModel ->
            config.view mainModel
                |> Html.map MainMsg
