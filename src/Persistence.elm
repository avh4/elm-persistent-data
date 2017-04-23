module Persistence
    exposing
        ( Program
        , Config
        , Model
        , Msg
        , uimsg
        , program
        , programWithNavigation
        , PersistenceState(..)
        , current
        )

{-|


## Creating a persistent program

@docs Config, Program, program, programWithNavigation


## Stuff you shouldn't normally need

@docs Model, Msg, uimsg, PersistenceState, current

-}

import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)
import Persistence.Batch as Batch
import Storage exposing (Storage)
import Storage.Hash as Hash exposing (Hash)
import Navigation


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
    , localCache :
        Maybe
            { encoder : data -> Json.Encode.Value
            , decoder : Decoder data
            , store : Storage.CacheStore
            }
    }


{-| A `Program` type alias for persistent programs.

    main : Persistence.Program Flags Data Event State Msg

-}
type alias Program flags data event state msg =
    Platform.Program flags (Model data state) (Msg event msg)


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


init :
    Config data event state msg
    -> ( Model data state, Cmd (Msg event msg) )
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
        , case config.localCache of
            Nothing ->
                readRoot config

            Just cacheConfig ->
                Task.perform ReadCache cacheConfig.store.read
        ]
    )


readRoot : Config data event state msg -> Cmd (Msg event msg)
readRoot config =
    Task.attempt ReadRoot <| config.storage.refs.read (config.appId ++ ".root-v1")


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
type Msg event msg
    = UiMsg msg
    | ReadRoot (Result String (Maybe Hash))
    | ReadBatch Hash (List (List event)) (Result String (Maybe String))
    | WriteBatch (Result String Hash)
    | WriteRoot Hash (Result String ())
    | ReadCache (Maybe String)
    | WriteCache ()


{-| Not sure if this should be exposed... it's needed for testing, though
-}
uimsg : msg -> Msg event msg
uimsg =
    UiMsg


writeRoot : Config data event state msg -> Maybe Hash -> Hash -> Task String ()
writeRoot config previousRoot lastBatchId =
    config.storage.refs.write (config.appId ++ ".root-v1") previousRoot lastBatchId


readBatch : Config data event state msg -> Hash -> List (List event) -> Cmd (Msg event msg)
readBatch config batch whenDone =
    config.storage.content.read batch
        |> Task.attempt (ReadBatch batch whenDone)


writeBatch : Config data event state msg -> Maybe Hash -> List event -> ( Hash, Cmd (Msg event msg) )
writeBatch config parent events =
    let
        json =
            Batch.encoder config.data.encoder { events = events, parent = parent }
                |> Json.Encode.encode 0
    in
        ( Hash.ofString json
        , config.storage.content.write json
            |> Task.attempt WriteBatch
        )


writeToCache : Config data event state msg -> Hash -> data -> Cmd (Msg event msg)
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


update :
    Config data event state msg
    -> Msg event msg
    -> Model data state
    -> ( Model data state, Cmd (Msg event msg) )
update config msg (Model model) =
    case msg of
        UiMsg m ->
            let
                ( newUi, uiCmd, events ) =
                    config.ui.update model.data m model.ui

                ( newData, writeCmd, cacheCmd ) =
                    case events of
                        [] ->
                            ( model.data, Cmd.none, Cmd.none )

                        _ ->
                            let
                                newData =
                                    List.foldl config.data.update model.data events

                                ( expectedHash, writeCmd_ ) =
                                    writeBatch config model.root events
                            in
                                ( newData
                                , writeCmd_
                                , writeToCache config expectedHash newData
                                )
            in
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

        ReadRoot (Ok Nothing) ->
            ( Model { model | loaded = True, root = Nothing }
            , Cmd.none
            )

        ReadRoot (Ok (Just lastBatchName)) ->
            if Just lastBatchName == model.root then
                ( Model { model | loaded = True }, Cmd.none )
            else
                ( Model { model | root = Just lastBatchName }
                , readBatch config lastBatchName []
                )

        ReadRoot (Err message) ->
            ( Model
                { model
                    | errors = ("Error reading root: " ++ message) :: model.errors
                }
            , Cmd.none
            )

        ReadBatch id whenDone (Ok (Just batchJson)) ->
            case Json.Decode.decodeString (Batch.decoder config.data.decoder) batchJson of
                Ok batch ->
                    case batch.parent of
                        Nothing ->
                            ( Model
                                { model
                                    | loaded = True
                                    , data =
                                        (batch.events :: whenDone)
                                            |> List.concat
                                            |> List.foldl config.data.update model.data
                                }
                            , Cmd.none
                            )

                        Just parent ->
                            ( Model model, readBatch config parent (batch.events :: whenDone) )

                Err message ->
                    ( Model
                        { model
                            | errors = ("Error decoding batch " ++ toString id ++ ": " ++ message) :: model.errors
                        }
                    , Cmd.none
                    )

        ReadBatch id _ (Ok Nothing) ->
            ( Model
                { model
                    | errors = ("Fatal error: a known batch of events is missing from the storage backend: " ++ toString id) :: model.errors
                }
            , Cmd.none
            )

        ReadBatch id _ (Err message) ->
            ( Model
                { model
                    | errors = ("Error reading batch " ++ toString id ++ ": " ++ message) :: model.errors
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

        ReadCache Nothing ->
            ( Model model, readRoot config )

        ReadCache (Just json) ->
            ( case config.localCache of
                Nothing ->
                    Model model

                Just cacheConfig ->
                    let
                        decoder =
                            Json.Decode.map2 CacheRecord
                                (Json.Decode.field "root" <| Hash.decode)
                                (Json.Decode.field "data" <| cacheConfig.decoder)
                    in
                        case Json.Decode.decodeString decoder json of
                            Ok result ->
                                Model
                                    { model
                                        | data = result.data
                                        , root = Just result.root
                                    }

                            Err _ ->
                                Model model
            , readRoot config
            )

        WriteCache () ->
            ( Model model, Cmd.none )


type alias CacheRecord data =
    { root : Hash
    , data : data
    }


subscriptions :
    Config data event state msg
    -> Model data state
    -> Sub (Msg event msg)
subscriptions config =
    -- TODO: add tests for this
    \(Model model) ->
        if model.loaded && model.errors == [] then
            config.ui.subscriptions model.data model.ui
                |> Sub.map UiMsg
        else
            Sub.none


view :
    Config data event state msg
    -> Model data state
    -> Html (Msg event msg)
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
    -> Program Never data event state msg
program config =
    Html.program
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
    (Navigation.Location -> msg)
    -> Config data event state msg
    -> Program Never data event state msg
programWithNavigation locationToMessage config =
    Navigation.program (locationToMessage >> UiMsg)
        { init =
            \location ->
                init config
                    |> -- TODO something's probably wrong if the initial location
                       -- (or actually any location change) causes a persistable event
                       -- probably the API should be changes to prevent that possibility
                       programAndThen (update config (locationToMessage location |> UiMsg))
        , update = update config
        , subscriptions = subscriptions config
        , view = view config
        }
