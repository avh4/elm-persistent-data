module Persistence exposing (PersistenceModel, PersistenceMsg, uimsg, program, PersistenceState(..), current)

import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)
import Sha256


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
    , read : String -> Task String (Maybe String)
    , write :
        String -> String -> Task String ()
        -- , maxBatchSize/maxBlobSize : Int
    }


type PersistenceModel data state
    = PersistenceModel
        { data : data
        , ui : state
        , loaded : Bool
        , errors : List String
        }


init :
    Config data event state msg
    -> ( PersistenceModel data state, Cmd (PersistenceMsg event msg) )
init config =
    ( PersistenceModel
        { data = config.initApp
        , ui = Tuple.first config.initUi
        , loaded = False
        , errors = []
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
    | ReadBatch String (Result String (Maybe String))
    | WriteBatch (Result String ())


{-| Not sure if this should be exposed... it's needed for testing, though
-}
uimsg : msg -> PersistenceMsg event msg
uimsg =
    UiMsg


writeBatch : Config data event state msg -> List event -> Task String ()
writeBatch config events =
    let
        json =
            [ ( "events"
              , List.map config.encoder events
                    |> Json.Encode.list
              )
            ]
                |> Json.Encode.object
                |> Json.Encode.encode 0

        key =
            "sha256-" ++ Sha256.sha256 json
    in
        config.write key json


update :
    Config data event state msg
    -> PersistenceMsg event msg
    -> PersistenceModel data state
    -> ( PersistenceModel data state, Cmd (PersistenceMsg event msg) )
update config msg (PersistenceModel model) =
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
                            , writeBatch config [ ev ]
                                |> Task.attempt WriteBatch
                            )
            in
                ( PersistenceModel
                    { model
                        | ui = newUi
                        , data = newData
                    }
                , writeCmd
                  -- TODO: use uiCmd
                )

        ReadRoot (Ok Nothing) ->
            ( PersistenceModel { model | loaded = True }
            , Cmd.none
            )

        ReadRoot (Ok (Just lastBatchName)) ->
            ( PersistenceModel model
            , Task.attempt (ReadBatch lastBatchName) <| config.read lastBatchName
            )

        ReadRoot (Err message) ->
            ( PersistenceModel
                { model
                    | errors = ("Error reading root: " ++ message) :: model.errors
                }
            , Cmd.none
            )

        ReadBatch id (Ok (Just batchJson)) ->
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
                    ( PersistenceModel
                        { model
                            | errors = ("Error decoding batch " ++ id ++ ": " ++ message) :: model.errors
                        }
                    , Cmd.none
                    )

        ReadBatch _ (Ok Nothing) ->
            Debug.crash "TODO: this is a fatal loading error; a known batch of events is missing"

        ReadBatch id (Err message) ->
            ( PersistenceModel
                { model
                    | errors = ("Error reading batch " ++ id ++ ": " ++ message) :: model.errors
                }
            , Cmd.none
            )

        WriteBatch (Ok ()) ->
            ( PersistenceModel model
            , Cmd.none
            )

        WriteBatch (Err _) ->
            Debug.crash "TODO: WriteBatch Err"


view :
    Config data event state msg
    -> PersistenceModel data state
    -> Html (PersistenceMsg event msg)
view config (PersistenceModel model) =
    if model.errors /= [] then
        Html.map never (config.errorView model.errors)
    else if model.loaded == False then
        Html.map never config.loadingView
    else
        config.view model.data model.ui
            |> Html.map UiMsg


program :
    Config data event state msg
    -> Program Never (PersistenceModel data state) (PersistenceMsg event msg)
program config =
    Html.program
        { init = init config
        , update = update config
        , subscriptions = \_ -> Sub.none
        , view = view config
        }
