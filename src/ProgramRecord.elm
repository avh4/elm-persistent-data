module ProgramRecord exposing
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

import Browser
import Html exposing (Html)
import Url exposing (Url)


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
    | WithLocation (Url -> init) (Url -> msg)
    | WithBoth (flags -> Url -> init) (Url -> msg)


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
                    WithBoth (\b a -> f a b) onLoc

                WithBoth f onLoc ->
                    WithBoth (\flags loc -> f flags loc flags) onLoc
    in
    { init = fixInit r.init
    , update = r.update
    , subscriptions = r.subscriptions
    , view = r.view
    }


applyInit : ProgramType flags init msg -> Maybe flags -> Url -> init
applyInit init flags location =
    case ( flags, init ) of
        ( Nothing, NoArgs value ) ->
            value

        ( Nothing, WithLocation f _ ) ->
            f location

        ( Nothing, _ ) ->
            Debug.todo "Program requires flags, but none were provided"

        ( Just fl, WithFlags f ) ->
            f fl

        ( Just fl, WithBoth f _ ) ->
            f fl location

        ( Just _, _ ) ->
            Debug.todo "Program has flags=Never, but flags were provided"


getLocationChange : ProgramType flags init msg -> Maybe (Url -> msg)
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


toProgram : ProgramRecord () Never model msg -> Platform.Program Url model msg
toProgram record =
    case record.init of
        NoArgs value ->
            Browser.element
                { init = \_ -> value |> handleNever
                , update = record.update |> c2 handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithFlags init ->
            Browser.element
                { init = always () >> init >> handleNever
                , update = record.update |> c2 handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithLocation init onLocationChange ->
            let
                _ =
                    Debug.log "TODO: navigation events are not handled"
            in
            Browser.element
                { init = init >> handleNever
                , update = record.update |> c2 handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithBoth init onLocationChange ->
            let
                _ =
                    Debug.log "TODO: navigation events are not handled"
            in
            Browser.element
                { init = init () >> handleNever
                , update = record.update |> c2 handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }


c2 : (c -> d) -> (a -> b -> c) -> (a -> b -> d)
c2 f y a b =
    y a b |> f


htmlProgram :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> ProgramRecord () Never model msg
htmlProgram config =
    { init = NoArgs (Err config.init)
    , update = config.update |> c2 Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


navigationProgram :
    (Url -> msg)
    ->
        { init : Url -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> ProgramRecord Never Never model msg
navigationProgram onLocationChange config =
    { init = WithLocation (config.init >> Err) onLocationChange
    , update = config.update |> c2 Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


navigationProgramWithFlags :
    (Url -> msg)
    ->
        { init : flags -> Url -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> ProgramRecord flags Never model msg
navigationProgramWithFlags onLocationChange config =
    { init = WithBoth (config.init |> c2 Err) onLocationChange
    , update = config.update |> c2 Err
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
