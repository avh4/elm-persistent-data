module Persistence.Simple exposing
    ( Config, program, programWithNavigation, Program
    , devProgram, devProgramWithNavigation
    , debugProgram, DebugConfig
    )

{-| A simplified wrapper for Persistence.program.

NOTE: This module will probably go away before 1.0.0 and the functions here will replace the current functions
in `Persistence`.

TODO: how to let the user log out? Probably via intercepting a special URL fragment

@docs Config, program, programWithNavigation, Program


## Development

These versions enable features in the UI that are useful for developers.

@docs devProgram, devProgramWithNavigation


## Even simpler programs

@docs debugProgram, DebugConfig

-}

import Browser
import Browser.Navigation as Navigation
import Debug.Control
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder)
import Json.Encode
import Persistence
import Persistence.SimpleAuth as SimpleAuth
import Persistence.SimpleStorageConfig as StorageConfig
import Storage exposing (Storage)
import Storage.Cache
import Storage.Hash
import Storage.Task
import Task exposing (Task)
import Url exposing (Url)


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
type alias Config flags data event state msg =
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
        { init : flags -> ( state, Cmd msg )
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
    = LoadingAuth Navigation.Key Url
    | AuthUi Navigation.Key Url SimpleAuth.Model
    | MainApp Navigation.Key (Persistence.Config data event state msg) (Persistence.Model data state)


type Msg data event msg
    = NoOp
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | LoadedAuth (Maybe String)
    | AuthUiMsg SimpleAuth.Msg
    | DebugNoAuth
    | MainAppMsg (Persistence.Msg data event msg)


{-| -}
program :
    Config () data event state msg
    -> Platform.Program () (Model data event state msg) (Msg data event msg)
program config =
    program_
        False
        Nothing
        (always ())
        (Just
            { encoder = config.dataCache.encoder
            , decoder = config.dataCache.decoder
            , store =
                { read = config.localStorage.get ".data"
                , write = config.localStorage.add ".data"
                }
            }
        )
        config


{-| -}
programWithNavigation :
    (Url -> msg)
    -> Config Url data event state msg
    -> Platform.Program () (Model data event state msg) (Msg data event msg)
programWithNavigation onLocationChange config =
    program_
        False
        (Just onLocationChange)
        identity
        (Just
            { encoder = config.dataCache.encoder
            , decoder = config.dataCache.decoder
            , store =
                { read = config.localStorage.get ".data"
                , write = config.localStorage.add ".data"
                }
            }
        )
        config


{-| -}
devProgram :
    Config () data event state msg
    -> Platform.Program () (Model data event state msg) (Msg data event msg)
devProgram config =
    program_
        True
        Nothing
        (always ())
        (Just
            { encoder = config.dataCache.encoder
            , decoder = config.dataCache.decoder
            , store =
                { read = config.localStorage.get ".data"
                , write = config.localStorage.add ".data"
                }
            }
        )
        config


{-| -}
devProgramWithNavigation :
    (Url -> msg)
    -> Config Url data event state msg
    -> Platform.Program () (Model data event state msg) (Msg data event msg)
devProgramWithNavigation onLocationChange config =
    program_
        True
        (Just onLocationChange)
        identity
        (Just
            { encoder = config.dataCache.encoder
            , decoder = config.dataCache.decoder
            , store =
                { read = config.localStorage.get ".data"
                , write = config.localStorage.add ".data"
                }
            }
        )
        config


{-| Type alias for `Persistence.Simple.program`
-}
type alias Program flags data event state msg =
    Platform.Program flags (Model data event state msg) (Msg data event msg)


program_ :
    Bool
    -> Maybe (Url -> msg)
    -> (Url -> flags)
    -> Maybe (Persistence.LocalCache data)
    -> Config flags data event state msg
    -> Program () data event state msg
program_ devMode onLocationChange makeFlags localCache config =
    let
        realConfig location storage =
            { appId = config.appId
            , data = config.data
            , ui =
                { init = config.ui.init (makeFlags location)
                , update = config.ui.update
                , subscriptions = config.ui.subscriptions
                , view = config.ui.view
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
                Storage.Cache.cache
                    (Storage.Task.storage config.localStorage)
                    storage
            , localCache = localCache
            }

        startApp key location storage =
            let
                persConfig =
                    realConfig location storage
            in
            Persistence.init persConfig
                |> Tuple.mapFirst (MainApp key persConfig)
                |> Tuple.mapSecond (Cmd.map MainAppMsg)

        startAuth key location =
            SimpleAuth.init
                { dropboxAppKey = config.serviceAppKeys.dropbox
                }
                key
                location
                |> handleAuthUiResult key location

        handleAuthUiResult key location result =
            case result of
                Err continue ->
                    continue
                        |> Tuple.mapFirst (AuthUi key location)
                        |> Tuple.mapSecond (Cmd.map AuthUiMsg)

                Ok ( storageConfig, cmd ) ->
                    let
                        persistAuth =
                            storageConfig
                                |> StorageConfig.encode
                                |> Json.Encode.encode 0
                                |> config.localStorage.add ".auth"
                                |> Task.perform (\() -> NoOp)

                        ( newModel, persCmds ) =
                            startApp key location (StorageConfig.toStorage storageConfig)
                    in
                    ( newModel
                    , Cmd.batch
                        [ persCmds
                        , persistAuth
                        , cmd
                        ]
                    )
    in
    Browser.application
        { init =
            \() location key ->
                ( LoadingAuth key location
                , config.localStorage.get ".auth"
                    |> Task.perform LoadedAuth
                )
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        , update =
            \msg model ->
                case ( msg, model ) of
                    ( NoOp, _ ) ->
                        ( model, Cmd.none )

                    ( UrlChange location, LoadingAuth key _ ) ->
                        ( LoadingAuth key location, Cmd.none )

                    ( UrlChange location, MainApp key appConfig appModel ) ->
                        case onLocationChange of
                            Just appMsg ->
                                Persistence.update appConfig (Persistence.uimsg <| appMsg location) appModel
                                    |> Tuple.mapFirst (MainApp key appConfig)
                                    |> Tuple.mapSecond (Cmd.map MainAppMsg)

                            Nothing ->
                                ( MainApp key appConfig appModel, Cmd.none )

                    ( UrlChange _, _ ) ->
                        ( model, Cmd.none )

                    ( UrlRequest _, _ ) ->
                        Debug.todo ("TODO: " ++ Debug.toString msg)

                    ( LoadedAuth Nothing, LoadingAuth key location ) ->
                        -- No auth data was stored, so ask the user to log in
                        startAuth key location

                    ( LoadedAuth (Just string), LoadingAuth key location ) ->
                        case Json.Decode.decodeString StorageConfig.decoder string of
                            Err message ->
                                -- Assuming no one else wrote to the ".auth" key,
                                -- this means that an older version of the code wrote something we can't read.
                                -- That means there's a bug in elm-persistent storage, and there's nothing the user or the app developer can do,
                                -- So we simply log it (to aid debugging) and pretend no auth data is stored
                                -- (which will force the user to log in again)
                                message
                                    |> Debug.log "Failed to decode stored auth info"
                                    |> always (startAuth key location)

                            Ok storageConfig ->
                                startApp key location (StorageConfig.toStorage storageConfig)

                    ( LoadedAuth _, _ ) ->
                        -- Got auth data, but auth already finished; just ignore it
                        ( model, Cmd.none )

                    ( AuthUiMsg authMsg, AuthUi key location authModel ) ->
                        SimpleAuth.update authMsg authModel
                            |> handleAuthUiResult key location

                    ( DebugNoAuth, AuthUi key location authModel ) ->
                        startApp key location fakeStorage

                    ( MainAppMsg appMsg, MainApp key appConfig appModel ) ->
                        Persistence.update appConfig appMsg appModel
                            |> Tuple.mapFirst (MainApp key appConfig)
                            |> Tuple.mapSecond (Cmd.map MainAppMsg)

                    ( MainAppMsg _, _ ) ->
                        Debug.todo "Internal error: We got a main app message before we started the main app!  This should never be possible!"

                    ( AuthUiMsg _, LoadingAuth _ _ ) ->
                        Debug.todo "Internal error: We got an auth ui message before we started the auth ui!  This should never be possible!"

                    ( DebugNoAuth, LoadingAuth _ _ ) ->
                        Debug.todo "Internal error: We got an auth ui message before we started the auth ui!  This should never be possible!"

                    ( _, MainApp _ _ _ ) ->
                        -- An auth UI message came in after auth finished; just ignore it
                        ( model, Cmd.none )
        , subscriptions =
            \model ->
                case model of
                    LoadingAuth _ _ ->
                        Sub.none

                    AuthUi _ _ _ ->
                        Sub.none

                    MainApp _ appConfig appModel ->
                        Persistence.subscriptions appConfig appModel
                            |> Sub.map MainAppMsg
        , view =
            \model ->
                { title = ""
                , body =
                    [ case model of
                        LoadingAuth _ _ ->
                            -- This should be nearly instantaneous (just a read to localstorage, so no need to show anything)
                            Html.text ""

                        AuthUi _ _ authModel ->
                            Html.div []
                                [ SimpleAuth.view authModel
                                    |> Html.map AuthUiMsg
                                , if devMode then
                                    Html.button
                                        [ onClick DebugNoAuth
                                        ]
                                        [ Html.text "DEBUG: no auth" ]

                                  else
                                    Html.text ""
                                ]

                        MainApp _ appConfig appModel ->
                            Persistence.view appConfig appModel
                                |> Html.map MainAppMsg
                    ]
                }
        }


fakeStorage : Storage
fakeStorage =
    { refs =
        { read = \_ -> Task.succeed Nothing
        , write = \_ _ _ -> Task.succeed ()
        }
    , content =
        { read = \_ -> Task.fail "Using fake storage"
        , write = \content -> Task.succeed (Storage.Hash.ofString content)
        }
    }


{-| -}
type alias DebugConfig data event =
    { appId : String
    , data :
        { init : data
        , update : event -> data -> data
        , encoder : event -> Json.Encode.Value
        , decoder : Decoder event
        }
    , ui : Debug.Control.Control event
    , serviceAppKeys :
        { dropbox : Maybe String
        }
    }


{-| An extremely simple way to create a program to test out your domain logic.
This will provide an extremely simple UI allowing you to add events and see the resulting domain state.
-}
debugProgram :
    DebugConfig data event
    -> Program () data event (Debug.Control.Control event) (Maybe (Debug.Control.Control event))
debugProgram config =
    program_
        False
        Nothing
        (always ())
        Nothing
        { appId = config.appId
        , data = config.data
        , dataCache =
            { encoder = always Json.Encode.null
            , decoder = Json.Decode.fail "Persistence.Simple.debugProgram does not implement a data decoder.  Use Persistence.Simple.program instead if you want computed date caching."
            }
        , ui =
            { init = \() -> ( config.ui, Cmd.none )
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
        , localStorage =
            { get = \_ -> Task.succeed Nothing
            , add = \_ _ -> Task.succeed ()
            }
        , serviceAppKeys = config.serviceAppKeys
        }


debugView : String -> data -> Debug.Control.Control event -> Html (Maybe (Debug.Control.Control event))
debugView appId data control =
    Html.div []
        [ Html.h2 [] [ Html.text appId ]
        , Html.h3 [] [ Html.text "data" ]
        , Html.text (Debug.toString data)
        , Html.h3 [] [ Html.text "new event" ]
        , Debug.Control.view Just control
        , Html.button [ onClick Nothing ] [ Html.text "Add" ]
        ]


programAndThen : (a -> ( b, Cmd msg )) -> ( a, Cmd msg ) -> ( b, Cmd msg )
programAndThen next ( a, cmdA ) =
    let
        ( b, cmdB ) =
            next a
    in
    ( b, Cmd.batch [ cmdA, cmdB ] )
