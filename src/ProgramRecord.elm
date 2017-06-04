module ProgramRecord
    exposing
        ( ProgramRecord
        , ProgramType(..)
        , applyInit
        , completableProgram
        , getLocationChange
        , htmlProgram
        , navigationProgram
        , toProgram
        , withFlags
        )

import Html exposing (Html)
import Navigation exposing (Location)


{-| `update` and `init` return a `Result` where:

  - `Ok` means auth is finished; the provided value will be used to initialize the main program
  - `Err` means auth did not finish; the provided value is the new model and Cmd for the auth program

-}
type alias ProgramRecord flags done model msg =
    ProgramRecord_ flags (Result ( model, Cmd msg ) done) done model msg


type alias ProgramWithFlags flags done model msg =
    ProgramRecord_ flags (flags -> Result ( model, Cmd msg ) done) done model msg


type alias ProgramRecord_ flags init done model msg =
    { init : ProgramType flags init msg
    , update : msg -> model -> Result ( model, Cmd msg ) done
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type ProgramType flags init msg
    = NoArgs init
    | WithFlags (flags -> init)
    | WithLocation (Location -> init) (Location -> msg)
    | WithBoth (flags -> Location -> init) (Location -> msg)


withFlags :
    ProgramWithFlags flags done model msg
    -> ProgramRecord flags done model msg
withFlags r =
    let
        fixInit :
            ProgramType flags (flags -> Result ( model, Cmd msg ) done) msg
            -> ProgramType flags (Result ( model, Cmd msg ) done) msg
        fixInit i =
            case i of
                NoArgs f ->
                    WithFlags f

                WithFlags f ->
                    WithFlags <| \flags -> f flags flags

                WithLocation f onLoc ->
                    WithBoth (flip f) onLoc

                WithBoth f onLoc ->
                    WithBoth (\flags loc -> f flags loc flags) onLoc
    in
    { init = fixInit r.init
    , update = r.update
    , subscriptions = r.subscriptions
    , view = r.view
    }


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


completableProgram :
    { init : Result ( model, Cmd msg ) done
    , update : msg -> model -> Result ( model, Cmd msg ) done
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> ProgramRecord Never done model msg
completableProgram config =
    { init = NoArgs config.init
    , update = config.update
    , subscriptions = config.subscriptions
    , view = config.view
    }


handleNever : Result a Never -> a
handleNever r =
    case r of
        Ok a ->
            never a

        Err x ->
            x
