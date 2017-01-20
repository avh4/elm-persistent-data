module TestApp exposing (Data, Event, UiState, Msg(..), program)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Persistence
import Json.Decode
import Json.Encode
import Storage exposing (Storage)
import Storage.Debug
import Storage.ExampleServer


type alias Data =
    { list : List String
    }


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
                    Json.Decode.fail ("Not valid pattern for decoder to Event. Pattern: " ++ (toString tag))
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


updateUi : Data -> Msg -> UiState -> ( UiState, Cmd Msg, Maybe Event )
updateUi data msg state =
    case msg of
        Typed string ->
            ( { state | input = string }
            , Cmd.none
            , Nothing
            )

        Add ->
            if state.input == "" then
                ( state, Cmd.none, Nothing )
            else
                ( { state | input = "" }
                , Cmd.none
                , Just <| AddItem state.input
                )


view : Data -> UiState -> Html Msg
view data state =
    Html.div []
        [ Html.ul []
            (List.map (\i -> Html.li [] [ Html.text i ]) data.list)
        , Html.hr [] []
        , Html.form [ Html.Events.onSubmit Add ]
            [ Html.input
                [ Html.Events.onInput Typed
                , Html.Attributes.value state.input
                ]
                []
            , Html.button [] [ Html.text "Add" ]
            ]
        ]


program : Storage -> Persistence.Program Never Data Event UiState Msg
program storage =
    Persistence.program
        { data =
            { init = { list = [] }
            , update = update
            , decoder = decoder
            , encoder = encoder
            }
        , ui =
            { init = ( { input = "" }, Cmd.none )
            , update = updateUi
            , subscriptions = \_ _ -> Sub.none
            , view = view
            }
        , loadingView = Html.text "Loading..."
        , errorView =
            \errors ->
                Html.div []
                    [ Html.text "Errors:"
                    , errors |> List.map (\i -> Html.li [] [ Html.text i ]) |> Html.ul []
                    ]
        , storage = storage
        }


main : Persistence.Program Never Data Event UiState Msg
main =
    program
        (Storage.Debug.storage "example-server (HTTP)" <|
            Storage.ExampleServer.storage
                -- If the backend server is running on a different host or port,
                -- the following string can be "https://myserver.tld/", etc.
                "/"
        )
