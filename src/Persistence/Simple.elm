module Persistence.Simple exposing (Config, DebugConfig, debugProgram, program)

{-| A simplified wrapper for Persistence.program.

NOTE: This module will probably go away before 1.0.0 and the functions here will replace the current functions
in `Persistence`.

TODO: how to let the user log out? Probably via intercepting a special URL fragment

@docs program, Config


## Even simpler programs

@docs debugProgram, DebugConfig

-}

import Debug.Control
import Dropbox
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder)
import Json.Encode
import Persistence
import Persistence.SimpleAuth as SimpleAuth
import Persistence.SimpleStorageConfig as StorageConfig exposing (StorageConfig)
import Storage.Cache
import Storage.Dropbox
import Storage.Task
import Task exposing (Task)


{-| Configuration for a [`Persistence.Simple.program`](#program).

  - appId: A unique identifier for your app.
    The recommended practice is to reverse a domain name you control.
    For example, `"com.google.mail"`, or `"io.github.avh4.calculator"`.
    This is used so that multiple apps can use the same storage configuration.
  - data: Configuration for the domain events your app will persist.
    You can think of this as a simplified program that cannot create Cmds, Subs or Html.
      - init: The initial domain state of your app for users who have no persisted data.
      - update: The update function for your domain data that applies new domain events to you domain state.
      - encoder: A JSON encoder for your domain events
      - decoder: A JSON decoder for your domain events
  - dataCache: Used to cache the computed state of your app's persistend events.
    This allows your app to load quickly.
      - encoder: A JSON encoder for your domain state
      - decoder: A JSON decoder for your domain state.
        Note that if this decoder fails, then elm-persistend-data will replay all events to recompute the user's state
        (This will result in slow loading times for your users.)
        However, this also means that you don't need to worry about migrating your domain state
        because the computed state can be regenerated at any time.
  - ui: Configuration for the UI and transient (not persisted) state of your app.
      - init: The initial state and Cmd for your UI
      - update: The update function for your UI.
        This differs slightly from a standard update function:
          - The current domain data for your app is provided as a separate parameter
            and is completely separate from the UI state.
          - In addition to returning the new UI state and Cmd,
            you can also return a list of domain events that will be persisted
            and also passed to the `data.update` function to update the domain state.
      - subscriptions: The subscriptions for your UI.
        This differs slightly from standard subscriptions in that you receive the domain state as a separate parameter.
      - view: The view for your UI.
        This differs slightly from a standard view in that you receive the domain state as a separate parameter.
  - localStorage: Since Elm currently does not provide a way to access local storage from within Elm,
    you must provide Tasks to allow elm-persistent-data to locally cache data.
    You can implement these tasks using [ports](https://guide.elm-lang.org/interop/javascript.html),
    or using the unpublished [elm-lang/persistent-cache](https://github.com/elm-lang/persistent-cache).
    (TODO: add examples of how to do this)
  - serviceAppKeys: Application client ids (a.k.a. app keys)
    You should provide as many of these as possible to allow your
    users the most choice in where their data is stored.

-}
type alias Config data event state msg =
    { appId : String
    , data :
        { init : data
        , update : event -> data -> data
        , encoder : event -> Json.Encode.Value
        , decoder : Decoder event
        }
    , dataCache :
        { encoder : data -> Json.Encode.Value
        , decoder : Decoder data
        }
    , ui :
        { init : ( state, Cmd msg )
        , update : data -> msg -> state -> ( state, Cmd msg, List event )
        , subscriptions : data -> state -> Sub msg
        , view : data -> state -> Html msg
        }
    , localStorage :
        { get : String -> Task Never (Maybe String)
        , add : String -> String -> Task Never ()
        }
    , serviceAppKeys :
        { dropbox : Maybe String
        }
    }


type Model data event state msg
    = LoadingAuth
    | AuthUi SimpleAuth.Model
    | MainApp (Persistence.Config data event state msg) (Persistence.Model data state)


type Msg data event msg
    = NoOp
    | LoadedAuth (Maybe String)
    | AuthUiMsg SimpleAuth.Msg
    | MainAppMsg (Persistence.Msg data event msg)


{-| -}
program :
    Config data event state msg
    -> Platform.Program Never (Model data event state msg) (Msg data event msg)
program config =
    let
        realConfig storage =
            { appId = config.appId
            , data = config.data
            , ui = config.ui
            , loadingView = Html.text "Loading (TODO: make this look nicer)"
            , errorView =
                \errors ->
                    Html.div []
                        [ Html.text "Errors:"
                        , errors |> List.map (\i -> Html.li [] [ Html.text i ]) |> Html.ul []
                        , Html.text "TODO: make this look nicer"
                        ]
            , storage =
                Storage.Cache.cache
                    (Storage.Task.storage config.localStorage)
                    storage
            , localCache =
                Just
                    { encoder = config.dataCache.encoder
                    , decoder = config.dataCache.decoder
                    , store =
                        { read = config.localStorage.get ".data"
                        , write = config.localStorage.add ".data"
                        }
                    }
            }

        startApp storageConfig =
            let
                persConfig =
                    realConfig (StorageConfig.toStorage storageConfig)
            in
            Persistence.init persConfig
                |> Tuple.mapFirst (MainApp persConfig)
                |> Tuple.mapSecond (Cmd.map MainAppMsg)

        startAuth =
            SimpleAuth.init
                { dropboxAppKey = config.serviceAppKeys.dropbox
                }
                |> Tuple.mapFirst AuthUi
                |> Tuple.mapSecond (Cmd.map AuthUiMsg)
    in
    Html.program
        { init =
            ( LoadingAuth
            , config.localStorage.get ".auth"
                |> Task.perform LoadedAuth
            )
        , update =
            \msg model ->
                case ( msg, model ) of
                    ( NoOp, _ ) ->
                        ( model, Cmd.none )

                    ( LoadedAuth Nothing, LoadingAuth ) ->
                        -- No auth data was stored, so ask the user to log in
                        startAuth

                    ( LoadedAuth (Just string), LoadingAuth ) ->
                        case Json.Decode.decodeString StorageConfig.decoder string of
                            Err message ->
                                -- Assuming no one else wrote to the ".auth" key,
                                -- this means that an older version of the code wrote something we can't read.
                                -- That means there's a bug in elm-persistent storage, and there's nothing the user or the app developer can do,
                                -- So we simply log it (to aid debugging) and pretend no auth data is stored
                                -- (which will force the user to log in again)
                                message
                                    |> Debug.log "Failed to decode stored auth info"
                                    |> always startAuth

                            Ok storageConfig ->
                                startApp storageConfig

                    ( LoadedAuth _, _ ) ->
                        -- Got auth data, but auth already finished; just ignore it
                        ( model, Cmd.none )

                    ( AuthUiMsg msg, AuthUi model ) ->
                        case SimpleAuth.update msg model of
                            Err continue ->
                                continue
                                    |> Tuple.mapFirst AuthUi
                                    |> Tuple.mapSecond (Cmd.map AuthUiMsg)

                            Ok storageConfig ->
                                let
                                    persistAuth =
                                        storageConfig
                                            |> StorageConfig.encode
                                            |> Json.Encode.encode 0
                                            |> config.localStorage.add ".auth"
                                            |> Task.perform (\() -> NoOp)

                                    ( newModel, persCmds ) =
                                        startApp storageConfig
                                in
                                ( newModel
                                , Cmd.batch
                                    [ persCmds
                                    , persistAuth
                                    ]
                                )

                    ( MainAppMsg msg, MainApp config model ) ->
                        Persistence.update config msg model
                            |> Tuple.mapFirst (MainApp config)
                            |> Tuple.mapSecond (Cmd.map MainAppMsg)

                    ( MainAppMsg _, _ ) ->
                        Debug.crash "Internal error: We got a main app message before we started the main app!  This should never be possible!"

                    ( AuthUiMsg _, LoadingAuth ) ->
                        Debug.crash "Internal error: We got an auth ui message before we started the auth ui!  This should never be possible!"

                    ( _, MainApp _ _ ) ->
                        -- An auth UI message came in after auth finished; just ignore it
                        ( model, Cmd.none )
        , subscriptions =
            \model ->
                case model of
                    LoadingAuth ->
                        Sub.none

                    AuthUi _ ->
                        Sub.none

                    MainApp config model ->
                        Persistence.subscriptions config model
                            |> Sub.map MainAppMsg
        , view =
            \model ->
                case model of
                    LoadingAuth ->
                        -- This should be nearly instantaneous (just a read to localstorage, so no need to show anything)
                        Html.text ""

                    AuthUi model ->
                        SimpleAuth.view model
                            |> Html.map AuthUiMsg

                    MainApp config model ->
                        Persistence.view config model
                            |> Html.map MainAppMsg
        }


{-| -}
type alias DebugConfig data event =
    { appId : String
    , dropboxAuthToken : String
    , data :
        { init : data
        , update : event -> data -> data
        , encoder : event -> Json.Encode.Value
        , decoder : Decoder event
        }
    , ui : Debug.Control.Control event
    }


{-| An extremely simple way to create a program to test out your domain logic.
This will provide an extremely simple UI allowing you to add events and see the resulting domain state.
-}
debugProgram : DebugConfig data event -> Persistence.Program Never data event (Debug.Control.Control event) (Maybe (Debug.Control.Control event))
debugProgram config =
    Persistence.program
        { appId = config.appId
        , data = config.data
        , ui =
            { init = ( config.ui, Cmd.none )
            , update =
                \data msg state ->
                    case msg of
                        Just control ->
                            ( control, Cmd.none, [] )

                        Nothing ->
                            ( config.ui, Cmd.none, [ Debug.Control.currentValue state ] )
            , subscriptions = \data state -> Sub.none
            , view = debugView config.appId
            }
        , loadingView = Html.text "Loading (TODO: make this look nicer)"
        , errorView =
            \errors ->
                Html.div []
                    [ Html.text "Errors:"
                    , errors |> List.map (\i -> Html.li [] [ Html.text i ]) |> Html.ul []
                    , Html.text "TODO: make this look nicer"
                    ]
        , storage =
            Storage.Dropbox.storage <|
                Dropbox.authorizationFromAccessToken config.dropboxAuthToken
        , localCache = Nothing
        }


debugView : String -> data -> Debug.Control.Control event -> Html (Maybe (Debug.Control.Control event))
debugView appId data control =
    Html.div []
        [ Html.h2 [] [ Html.text appId ]
        , Html.h3 [] [ Html.text "data" ]
        , Html.text (toString data)
        , Html.h3 [] [ Html.text "new event" ]
        , Debug.Control.view Just control
        , Html.button [ onClick Nothing ] [ Html.text "Add" ]
        ]
