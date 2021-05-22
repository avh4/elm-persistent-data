module TestApp exposing (Data, Event, Msg(..), UiState, appId, data, dataDecoder, dataEncoder, program, programRecord, ui)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Persistence
import ProgramRecord
import Storage exposing (Storage)
import Storage.Debug
import Storage.ExampleServer
import Url exposing (Url)


type alias Data =
    { list : List String
    }


dataEncoder : Data -> Json.Encode.Value
dataEncoder data_ =
    Json.Encode.object
        [ ( "list"
          , Json.Encode.list Json.Encode.string data_.list
          )
        ]


dataDecoder : Json.Decode.Decoder Data
dataDecoder =
    Json.Decode.map Data
        (Json.Decode.field "list" <| Json.Decode.list Json.Decode.string)


type Event
    = AddItem String


decoder : Json.Decode.Decoder Event
decoder =
    let
        dispatch : String -> Json.Decode.Decoder Event
        dispatch tag =
            case tag of
                "AddItem" ->
                    Json.Decode.map AddItem <|
                        Json.Decode.field "$0" Json.Decode.string

                _ ->
                    Json.Decode.fail ("Not valid pattern for decoder to Event. Pattern: " ++ Debug.toString tag)
    in
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen dispatch


encoder : Event -> Json.Encode.Value
encoder event =
    case event of
        AddItem name ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "AddItem" )
                , ( "$0", Json.Encode.string name )
                ]


update : Event -> Data -> Data
update msg model =
    case msg of
        AddItem name ->
            { model | list = name :: model.list }


type alias UiState =
    { input : String }


type Msg
    = Typed String
    | Add


updateUi : Data -> Msg -> UiState -> ( UiState, Cmd Msg, List Event )
updateUi _ msg state =
    case msg of
        Typed string ->
            ( { state | input = string }
            , Cmd.none
            , []
            )

        Add ->
            if state.input == "" then
                ( state, Cmd.none, [] )

            else
                ( { state | input = "" }
                , Cmd.none
                , [ AddItem state.input ]
                )


view : Data -> UiState -> Html Msg
view data_ state =
    Html.div []
        [ Html.h3 [] [ Html.text "Persistent TODO list" ]
        , Html.form [ Html.Events.onSubmit Add ]
            [ Html.input
                [ Html.Events.onInput Typed
                , Html.Attributes.value state.input
                ]
                []
            , Html.button [] [ Html.text "Add" ]
            ]
        , Html.hr [] []
        , Html.ul []
            (List.map (\i -> Html.li [] [ Html.text i ]) data_.list)
        ]


appId : String
appId =
    "io.github.avh4.elm-persistent-data.test-app"


data =
    { init = { list = [] }
    , update = update
    , decoder = decoder
    , encoder = encoder
    }


ui =
    { init = ( { input = "" }, Cmd.none )
    , update = updateUi
    , subscriptions = \_ _ -> Sub.none
    , view = view
    }


programRecord : Storage -> Maybe Storage.CacheStore -> Persistence.ProgramRecord () Data Event UiState Msg
programRecord storage cacheStore =
    Persistence.programRecord
        { data = data
        , ui = ui
        , loadingView = Html.text "Loading..."
        , errorView =
            \errors ->
                Html.div []
                    [ Html.text "Errors:"
                    , errors |> List.map (\i -> Html.li [] [ Html.text i ]) |> Html.ul []
                    ]
        , storage = storage
        , appId = appId
        , localCache =
            cacheStore
                |> Maybe.map
                    (\store ->
                        { encoder = dataEncoder
                        , decoder = dataDecoder
                        , store = store
                        }
                    )
        }


program : Storage -> Maybe Storage.CacheStore -> Persistence.Program Url Data Event UiState Msg
program storage cacheStore =
    programRecord storage cacheStore
        |> ProgramRecord.toProgram


main : Persistence.Program Url Data Event UiState Msg
main =
    program
        (Storage.Debug.storage "example-server (HTTP)" <|
            Storage.ExampleServer.storage
                -- If the backend server is running on a different host or port,
                -- the following string can be "https://myserver.tld/", etc.
                "/"
        )
        Nothing
