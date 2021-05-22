module Persistence exposing
    ( Config, LocalCache
    , Program, program, programWithNavigation, programRecord, ProgramRecord
    , Model, Msg, uimsg, PersistenceState(..), current
    , init, update, subscriptions, view
    )

{-|


## Creating a persistent program

@docs Config, LocalCache
@docs Program, program, programWithNavigation, programRecord, ProgramRecord


## Stuff you shouldn't normally need

@docs Model, Msg, uimsg, PersistenceState, current
@docs init, update, subscriptions, view

-}

import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode
import Persistence.Batch as Batch
import Persistence.Initializer as Init
import Process
import ProgramRecord
import Storage exposing (Storage)
import Storage.Hash as Hash exposing (Hash)
import Task exposing (Task)
import Url exposing (Url)


{-| Configuration for a persistent program.
-}
type alias Config data event state msg =
    { data :
        { init : data
        , update : event -> data -> data
        , decoder : Decoder event
        , encoder : event -> Json.Encode.Value
        }
    , ui :
        { init : ( state, Cmd msg )
        , update : data -> msg -> state -> ( state, Cmd msg, List event )
        , subscriptions : data -> state -> Sub msg
        , view : data -> state -> Html msg
        }
    , loadingView : Html Never
    , errorView : List String -> Html Never
    , storage : Storage
    , appId :
        -- This is used so that multiple apps can use the same storage configuration
        String
    , localCache : Maybe (LocalCache data)
    }


{-| Configuration for a persistent program's computed data cache.
-}
type alias LocalCache data =
    { encoder : data -> Json.Encode.Value
    , decoder : Decoder data
    , store : Storage.CacheStore
    }


{-| A `Program` type alias for persistent programs.

    main : Persistence.Program Flags Data Event State Msg

-}
type alias Program flags data event state msg =
    Platform.Program flags (Model data state) (Msg data event msg)


{-| A `ProgramRecord` type alias for persistent programs.
-}
type alias ProgramRecord flags data event state msg =
    ProgramRecord.ProgramRecord flags Never (Model data state) (Msg data event msg)


{-| The model for a persistence program.
-}
type Model data state
    = Model
        { data : data
        , ui : state
        , loaded : Bool
        , root : Maybe Hash
        , errors : List String
        }


{-| The `init` value for a Persistence.Program.

You should normally use `program` instead of using this directly.

-}
init :
    Config data event state msg
    -> ( Model data state, Cmd (Msg data event msg) )
init config =
    ( Model
        { data = config.data.init
        , ui = Tuple.first config.ui.init
        , loaded = False
        , root = Nothing
        , errors = []
        }
    , Cmd.batch
        [ Cmd.map UiMsg (Tuple.second config.ui.init)
        , startup config
            |> Task.attempt Startup
        ]
    )


startup : Config data event state msg -> Task String ( Maybe ( Hash, data ), Maybe Hash )
startup config =
    let
        decoder cacheDecoder =
            Json.Decode.map2 (\a b -> ( a, b ))
                (Json.Decode.field "root" Hash.decode)
                (Json.Decode.field "data" cacheDecoder)

        parseCache cacheDecoder json =
            Json.Decode.decodeString (decoder cacheDecoder) json
                |> Result.toMaybe
    in
    Task.map2 (\a b -> ( a, b ))
        (case config.localCache of
            Nothing ->
                Task.succeed Nothing

            Just cacheConfig ->
                cacheConfig.store.read
                    |> Task.mapError never
                    |> Task.map (Maybe.andThen (parseCache cacheConfig.decoder))
        )
        (readRootTask config)


readRootTask : Config data event state msg -> Task String (Maybe Hash)
readRootTask config =
    config.storage.refs.read (config.appId ++ ".root-v1")
        |> Task.andThen
            (\r ->
                case Maybe.map Hash.fromString r of
                    Nothing ->
                        Task.succeed Nothing

                    Just (Err message) ->
                        Task.fail message

                    Just (Ok hash) ->
                        Task.succeed (Just hash)
            )


{-| The externally-inspectable state of a persistent program.
-}
type PersistenceState data ui
    = Loading
    | Ready data ui


{-| Get the current state of a persistent program.
-}
current : Model data ui -> PersistenceState data ui
current (Model model) =
    case model.loaded of
        False ->
            Loading

        True ->
            Ready model.data model.ui


{-| The Msg type for a persistent program.
-}
type Msg data event msg
    = UiMsg msg
    | Startup (Result String ( Maybe ( Hash, data ), Maybe Hash ))
    | FetchBatch (Result String (Init.Next Hash data event))
    | WriteBatch (Result String Hash)
    | WriteRoot Hash (Result String ())
    | WriteCache ()


{-| Not sure if this should be exposed... it's needed for testing, though
-}
uimsg : msg -> Msg data event msg
uimsg =
    UiMsg


jsonTask : Decoder a -> Task String String -> Task String a
jsonTask decoder task =
    let
        parse json =
            case Json.Decode.decodeString decoder json of
                Err message ->
                    Task.fail (Json.Decode.errorToString message)

                Ok value ->
                    Task.succeed value
    in
    task |> Task.andThen parse


writeRoot : Config data event state msg -> Maybe Hash -> Hash -> Task String ()
writeRoot config previousRoot lastBatchId =
    config.storage.refs.write (config.appId ++ ".root-v1")
        (Maybe.map Hash.toString previousRoot)
        (Hash.toString lastBatchId)


writeBatch : Config data event state msg -> Maybe Hash -> List event -> Result String ( Hash, Cmd (Msg data event msg) )
writeBatch config parent events =
    let
        batch =
            Batch.encoder config.data.decoder config.data.encoder { events = events, parent = parent }
                |> Result.map (Json.Encode.encode 0)
    in
    case batch of
        Err message ->
            Err message

        Ok json ->
            Ok
                ( Hash.ofString json
                , config.storage.content.write json
                    |> Task.attempt WriteBatch
                )


writeToCache : Config data event state msg -> Hash -> data -> Cmd (Msg data event msg)
writeToCache config hash data =
    case config.localCache of
        Nothing ->
            Cmd.none

        Just cacheConfig ->
            Json.Encode.object
                [ ( "root", Hash.encode hash )
                , ( "data", cacheConfig.encoder data )
                ]
                |> Json.Encode.encode 0
                |> cacheConfig.store.write
                |> Task.perform WriteCache


{-| The `update` function for a Persistence.Program.

You should normally use `program` instead of using this directly.

-}
update :
    Config data event state msg
    -> Msg data event msg
    -> Model data state
    -> ( Model data state, Cmd (Msg data event msg) )
update config msg (Model model) =
    let
        handleInitResult result =
            case result of
                Init.Done rootId data ->
                    ( Model
                        { model
                            | loaded = True
                            , root = rootId
                            , data = data
                        }
                    , case rootId of
                        Nothing ->
                            Cmd.none

                        Just root ->
                            -- Don't actually need to write if root is the same as what is currently cached
                            writeToCache config root data
                    )

                Init.FetchBatch batchId continue ->
                    ( Model model
                    , config.storage.content.read batchId
                        |> jsonTask (Batch.decoder config.data.decoder)
                        |> Task.map (\batch -> continue <| Err ( batch.events, batch.parent ))
                        |> Task.attempt FetchBatch
                    )
    in
    case msg of
        UiMsg m ->
            let
                ( newUi, uiCmd, events ) =
                    config.ui.update model.data m model.ui

                result =
                    case events of
                        [] ->
                            Ok ( model.data, Cmd.none, Cmd.none )

                        _ ->
                            let
                                newData =
                                    List.foldl config.data.update model.data events
                            in
                            case writeBatch config model.root events of
                                Err message ->
                                    Err message

                                Ok ( expectedHash, writeCmd_ ) ->
                                    Ok
                                        ( newData
                                        , writeCmd_
                                        , writeToCache config expectedHash newData
                                        )
            in
            case result of
                Err message ->
                    ( Model
                        { model
                            | errors = ("Error writing events: " ++ message) :: model.errors
                        }
                    , Cmd.none
                    )

                Ok ( newData, writeCmd, cacheCmd ) ->
                    ( Model
                        { model
                            | ui = newUi
                            , data = newData
                        }
                    , Cmd.batch
                        [ writeCmd
                        , Cmd.map UiMsg uiCmd
                        , cacheCmd
                        ]
                    )

        Startup (Ok ( cachedData, latestRoot )) ->
            Init.init
                { cachedData = cachedData |> Maybe.map (\( root, data ) -> { root = root, data = data })
                , latestRoot = latestRoot
                , emptyData = config.data.init
                , fold = config.data.update
                }
                |> handleInitResult

        Startup (Err message) ->
            ( Model
                { model
                    | errors = ("Error reading root: " ++ message) :: model.errors
                }
            , Process.sleep 5000
                |> Task.andThen (\_ -> startup config)
                |> Task.attempt Startup
            )

        FetchBatch (Ok next) ->
            handleInitResult next

        FetchBatch (Err message) ->
            -- TODO: retry
            ( Model
                { model
                    | errors = ("Error reading batch: " ++ message) :: model.errors
                }
            , Cmd.none
            )

        WriteBatch (Ok hash) ->
            ( Model model
            , writeRoot config model.root hash
                |> Task.attempt (WriteRoot hash)
            )

        WriteBatch (Err message) ->
            ( Model
                { model
                    | errors =
                        ("Error writing batch: " ++ message) :: model.errors
                }
            , Cmd.none
            )

        WriteRoot newRoot (Ok ()) ->
            ( Model { model | root = Just newRoot }
            , Cmd.none
            )

        WriteRoot _ (Err message) ->
            ( Model
                { model
                    | errors =
                        ("Error writing root: " ++ message) :: model.errors
                }
            , Cmd.none
            )

        WriteCache () ->
            ( Model model, Cmd.none )


type alias CacheRecord data =
    { root : Hash
    , data : data
    }


{-| The `subscriptions` function for a Persistence.Program.

You should normally use `program` instead of using this directly.

-}
subscriptions :
    Config data event state msg
    -> Model data state
    -> Sub (Msg data event msg)
subscriptions config =
    -- TODO: add tests for this
    \(Model model) ->
        if model.loaded && model.errors == [] then
            config.ui.subscriptions model.data model.ui
                |> Sub.map UiMsg

        else
            Sub.none


{-| The `view` function for a Persistence.Program.

You should normally use `program` instead of using this directly.

-}
view :
    Config data event state msg
    -> Model data state
    -> Html (Msg data event msg)
view config (Model model) =
    if model.errors /= [] then
        Html.map never (config.errorView model.errors)

    else if model.loaded == False then
        Html.map never config.loadingView

    else
        config.ui.view model.data model.ui
            |> Html.map UiMsg


{-| Create a persistent program.
-}
program :
    Config data event state msg
    -> Program Url data event state msg
program config =
    ProgramRecord.toProgram <|
        programRecord config


{-| Create the functions that are needed to create a persistent program.

If possible, you should use [`program`](#program) instead.
You may need to use this if you need more control over how and when the persistent program is started
(such as if you need to do interactive authentication to create the `Storage` implementation.)

-}
programRecord :
    Config data event state msg
    -> ProgramRecord () data event state msg
programRecord config =
    ProgramRecord.htmlProgram
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        , view = view config
        }


programAndThen : (a -> ( b, Cmd msg )) -> ( a, Cmd msg ) -> ( b, Cmd msg )
programAndThen next ( a, cmdA ) =
    let
        ( b, cmdB ) =
            next a
    in
    ( b, Cmd.batch [ cmdA, cmdB ] )


{-| Create a persistent program with navigation (see elm-lang/navigation).
-}
programWithNavigation :
    (Url -> msg)
    -> Config data event state msg
    -> ProgramRecord Never data event state msg
programWithNavigation locationToMessage config =
    ProgramRecord.navigationProgram
        (locationToMessage >> UiMsg)
        { init =
            \location ->
                init config
                    |> -- TODO something's probably wrong if the initial location
                       -- (or actually any location change) causes a persistable event
                       -- probably the API should be changes to prevent that possibility.
                       -- See the alternative approach in Persistence.Simple where Config.init takes a param.
                       programAndThen (update config (locationToMessage location |> UiMsg))
        , update = update config
        , subscriptions = subscriptions config
        , view = view config
        }
