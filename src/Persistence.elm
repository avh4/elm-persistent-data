module Persistence
    exposing
        ( Program
        , Config
        , Model
        , Msg
        , uimsg
        , program
        , PersistenceState(..)
        , current
        )

{-|

## Creating a persistent program

@docs Config, Program, program

## Stuff you shouldn't normally need

@docs Model, Msg, uimsg, PersistenceState, current

-}

import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)
import Sha256
import Persistence.Batch as Batch
import Storage exposing (Storage)


{-| Configuration for a persistent program.
-}
type alias Config data event state msg =
    { initUi : ( state, Cmd msg )
    , initApp : data
    , update : event -> data -> data
    , updateUi : data -> msg -> state -> ( state, Cmd msg, Maybe event )
    , subscriptions : data -> state -> Sub msg
    , view : data -> state -> Html msg
    , loadingView : Html Never
    , errorView : List String -> Html Never
    , decoder : Decoder event
    , encoder : event -> Json.Encode.Value
    , storage : Storage
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
        , root : Maybe String
        , errors : List String
        }


init :
    Config data event state msg
    -> ( Model data state, Cmd (Msg event msg) )
init config =
    ( Model
        { data = config.initApp
        , ui = Tuple.first config.initUi
        , loaded = False
        , root = Nothing
        , errors = []
        }
    , Cmd.batch
        [ Cmd.map UiMsg (Tuple.second config.initUi)
        , Task.attempt ReadRoot <| config.storage.read "root-v1"
        ]
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
type Msg event msg
    = UiMsg msg
    | ReadRoot (Result String (Maybe String))
    | ReadBatch String (List (List event)) (Result String (Maybe String))
    | WriteBatch String (Result String String)
    | WriteRoot String (Result String ())


{-| Not sure if this should be exposed... it's needed for testing, though
-}
uimsg : msg -> Msg event msg
uimsg =
    UiMsg


writeRoot : Config data event state msg -> Maybe String -> String -> Task String ()
writeRoot config previousRoot lastBatchId =
    config.storage.writeRef "root-v1" previousRoot lastBatchId


readBatch : Config data event state msg -> String -> List (List event) -> Cmd (Msg event msg)
readBatch config batch whenDone =
    Task.attempt (ReadBatch batch whenDone) <| config.storage.read batch


writeBatch : Config data event state msg -> Maybe String -> List event -> Cmd (Msg event msg)
writeBatch config parent events =
    let
        json =
            Batch.encoder config.encoder { events = events, parent = parent }
                |> Json.Encode.encode 0

        key =
            "sha256-" ++ Sha256.sha256 json
    in
        config.storage.writeContent json
            |> Task.attempt (WriteBatch key)


update :
    Config data event state msg
    -> Msg event msg
    -> Model data state
    -> ( Model data state, Cmd (Msg event msg) )
update config msg (Model model) =
    case msg of
        UiMsg m ->
            let
                ( newUi, uiCmd, event ) =
                    config.updateUi model.data m model.ui

                ( newData, writeCmd ) =
                    case event of
                        Nothing ->
                            ( model.data, Cmd.none )

                        Just ev ->
                            ( config.update ev model.data
                            , writeBatch config model.root [ ev ]
                            )
            in
                ( Model
                    { model
                        | ui = newUi
                        , data = newData
                    }
                , writeCmd
                  -- TODO: use uiCmd
                )

        ReadRoot (Ok Nothing) ->
            ( Model { model | loaded = True, root = Nothing }
            , Cmd.none
            )

        ReadRoot (Ok (Just lastBatchName)) ->
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
            case Json.Decode.decodeString (Batch.decoder config.decoder) batchJson of
                Ok batch ->
                    case batch.parent of
                        Nothing ->
                            ( Model
                                { model
                                    | loaded = True
                                    , data =
                                        (batch.events :: whenDone)
                                            |> List.concat
                                            |> List.foldl config.update model.data
                                }
                            , Cmd.none
                            )

                        Just parent ->
                            ( Model model, readBatch config parent (batch.events :: whenDone) )

                Err message ->
                    ( Model
                        { model
                            | errors = ("Error decoding batch " ++ id ++ ": " ++ message) :: model.errors
                        }
                    , Cmd.none
                    )

        ReadBatch _ _ (Ok Nothing) ->
            Debug.crash "TODO: this is a fatal loading error; a known batch of events is missing"

        ReadBatch id _ (Err message) ->
            ( Model
                { model
                    | errors = ("Error reading batch " ++ id ++ ": " ++ message) :: model.errors
                }
            , Cmd.none
            )

        WriteBatch calculatedSha (Ok serverSha) ->
            -- TODO: verify serverSha matches what we calculated
            ( Model model
            , writeRoot config model.root calculatedSha
                |> Task.attempt (WriteRoot calculatedSha)
            )

        WriteBatch batchId (Err _) ->
            Debug.crash "TODO: WriteBatch Err"

        WriteRoot newRoot (Ok ()) ->
            ( Model { model | root = Just newRoot }
            , Cmd.none
            )

        WriteRoot _ (Err _) ->
            Debug.crash "TODO: WriteRoot Err"


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
        config.view model.data model.ui
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
        , subscriptions = \_ -> Sub.none
        , view = view config
        }
