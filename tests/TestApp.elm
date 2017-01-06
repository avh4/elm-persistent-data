module TestApp exposing (Msg(..), program)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Persistence
import Task
import Json.Decode
import Http


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
        , Html.input
            [ Html.Events.onInput Typed
            , Html.Attributes.value state.input
            ]
            []
        , Html.button [ Html.Events.onClick Add ] [ Html.text "Add" ]
        ]


program { read } =
    Persistence.program
        { initApp = { list = [] }
        , initUi = ( { input = "" }, Cmd.none )
        , update = update
        , updateUi = updateUi
        , subscriptions = \_ _ -> Sub.none
        , view = view
        , loadingView = Html.text "Loading..."
        , errorView =
            \errors ->
                Html.div []
                    [ Html.text "Errors:"
                    , errors |> List.map (\i -> Html.li [] [ Html.text i ]) |> Html.ul []
                    ]
        , decoder = decoder
        , read = read
        }


main =
    program
        { read =
            \key ->
                Http.getString ("/" ++ key)
                    |> Http.toTask
                    |> Task.map Just
                    |> Task.mapError toString
        }
