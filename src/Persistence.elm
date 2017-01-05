module Persistence exposing (PersistenceModel, PersistenceMsg, program, PersistenceState(..), current)

import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)


type alias Config data event state msg =
    { initUi : ( state, Cmd msg )
    , initApp : data
    , update : event -> data -> data
    , updateUi : data -> msg -> state -> ( state, Cmd msg, Maybe event )
    , subscriptions : data -> state -> Sub msg
    , view : data -> state -> Html msg
    , decoder :
        Decoder event
        -- , encoder : event -> Json.Encode.Value
    , read :
        String -> Task String (Maybe String)
        -- , write : String -> String -> Task String ()
        -- , maxBatchSize/maxBlobSize : Int
    }


type PersistenceModel data state
    = PersistenceModel
        { data : data
        , ui : state
        , loaded : Bool
        }


init :
    Config data event state msg
    -> ( PersistenceModel data state, Cmd (PersistenceMsg event msg) )
init config =
    ( PersistenceModel
        { data = config.initApp
        , ui = Tuple.first config.initUi
        , loaded = False
        }
    , Cmd.batch
        [ Cmd.map UiMsg (Tuple.second config.initUi)
        , Task.attempt ReadRoot <| config.read "root-v1"
        ]
    )


type PersistenceState data ui
    = Loading
    | Ready data ui


current : PersistenceModel data ui -> PersistenceState data ui
current (PersistenceModel model) =
    case model.loaded of
        False ->
            Loading

        True ->
            Ready model.data model.ui


type PersistenceMsg event msg
    = UiMsg msg
    | ReadRoot (Result String (Maybe String))
    | ReadBatch (Result String (Maybe String))


update :
    Config data event state msg
    -> PersistenceMsg event msg
    -> PersistenceModel data state
    -> ( PersistenceModel data state, Cmd (PersistenceMsg event msg) )
update config msg (PersistenceModel model) =
    case msg of
        UiMsg m ->
            let
                ( newUi, cmd, event ) =
                    config.updateUi model.data m model.ui
            in
                ( PersistenceModel
                    { model
                        | ui = newUi
                        , data =
                            case event of
                                Nothing ->
                                    model.data

                                Just ev ->
                                    config.update ev model.data
                    }
                , Cmd.none
                )

        ReadRoot (Ok Nothing) ->
            ( PersistenceModel { model | loaded = True }
            , Cmd.none
            )

        ReadRoot (Ok (Just lastBatchName)) ->
            ( PersistenceModel model
            , Task.attempt ReadBatch <| config.read lastBatchName
            )

        ReadRoot (Err _) ->
            Debug.crash "TODO"

        ReadBatch (Ok (Just batchJson)) ->
            case Json.Decode.decodeString (Json.Decode.field "events" <| Json.Decode.list config.decoder) batchJson of
                Ok events ->
                    ( PersistenceModel
                        { model
                            | loaded = True
                            , data = List.foldl config.update model.data events
                        }
                    , Cmd.none
                    )

                Err message ->
                    Debug.crash message

        ReadBatch (Ok Nothing) ->
            Debug.crash "TODO: this is a fatal loading error; a known batch of events is missing"

        ReadBatch (Err _) ->
            Debug.crash "TODO"


view :
    (data -> state -> Html msg)
    -> PersistenceModel data state
    -> Html (PersistenceMsg event msg)
view appView (PersistenceModel model) =
    appView model.data model.ui
        |> Html.map UiMsg


program :
    Config data event state msg
    -> Program Never (PersistenceModel data state) (PersistenceMsg event msg)
program config =
    Html.program
        { init = init config
        , update = update config
        , subscriptions = \_ -> Sub.none
        , view = view config.view
        }
