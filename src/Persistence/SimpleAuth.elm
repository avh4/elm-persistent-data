module Persistence.SimpleAuth exposing (Config, Model, Msg, init, update, view)

import Html exposing (Html)
import Html.Attributes exposing (defaultValue, placeholder)
import Html.Events exposing (onClick, onInput)
import Persistence.SimpleStorageConfig as StorageConfig exposing (StorageConfig)


type alias Config =
    { dropboxAppKey : Maybe String
    }


type Model
    = Model
        { step : Step
        , config : Config
        , dropboxAuthToken : String
        , dropboxClientId : String
        }


type Step
    = Choose
    | Dropbox
    | ChooseCustom
    | CustomDropboxAuthToken
    | CustomDropboxClientId


type Msg
    = ChooseDropbox String
    | ChooseChooseCustom
    | ChooseCustomDropboxAuthToken
    | ChooseCustomDropboxClientId
    | ChangeDropboxAuthToken String
    | CompleteCustomDropboxAuthToken String
    | ChangeDropboxClientId String


init : Config -> ( Model, Cmd Msg )
init config =
    ( Model
        { step = Choose
        , config = config
        , dropboxAuthToken = ""
        , dropboxClientId = ""
        }
    , Cmd.none
    )


update : Msg -> Model -> Result ( Model, Cmd Msg ) StorageConfig
update msg (Model model) =
    let
        continue newModel =
            Err ( Model newModel, Cmd.none )
    in
    case ( model.step, msg ) of
        ( _, ChooseDropbox clientId ) ->
            continue { model | step = Dropbox }

        ( _, ChooseChooseCustom ) ->
            continue { model | step = ChooseCustom }

        ( _, ChooseCustomDropboxAuthToken ) ->
            continue { model | step = CustomDropboxAuthToken }

        ( _, ChooseCustomDropboxClientId ) ->
            continue { model | step = CustomDropboxClientId }

        ( _, ChangeDropboxAuthToken new ) ->
            continue { model | dropboxAuthToken = new }

        ( _, CompleteCustomDropboxAuthToken authToken ) ->
            Ok (StorageConfig.Dropbox authToken)

        ( _, ChangeDropboxClientId new ) ->
            continue { model | dropboxClientId = new }


button : Msg -> String -> Html Msg
button msg label =
    Html.button
        [ onClick msg ]
        [ Html.text label ]


view : Model -> Html Msg
view (Model model) =
    case model.step of
        Choose ->
            Html.div []
                [ case model.config.dropboxAppKey of
                    Just clientId ->
                        button (ChooseDropbox clientId) "Dropbox"

                    Nothing ->
                        Html.text "(Dropbox not supported by the app developer)"
                , button ChooseChooseCustom "Advanced"
                ]

        Dropbox ->
            Html.text "TODO: Dropbox auth UI"

        ChooseCustom ->
            Html.div []
                [ Html.button
                    [ onClick ChooseCustomDropboxAuthToken ]
                    [ Html.text "Dropbox: provide an auth token" ]
                , Html.button
                    [ onClick ChooseCustomDropboxClientId ]
                    [ Html.text "Dropbox: provide a client id" ]
                ]

        CustomDropboxAuthToken ->
            Html.div []
                [ Html.text "For this advanced authentication method, you must have a Dropbox account.\n                             Then go to https://developers.dropbox.com and create a new App.\n                             On the new app's settings page, click \"Generate access token\"\n                             Copy the generated token here:"
                , Html.input
                    [ placeholder "Dropbox auth token"
                    , defaultValue model.dropboxAuthToken
                    , onInput ChangeDropboxAuthToken
                    ]
                    []
                , case String.trim model.dropboxAuthToken of
                    "" ->
                        -- TODO: show disabled button
                        Html.text ""

                    authToken ->
                        Html.button
                            [ onClick (CompleteCustomDropboxAuthToken authToken) ]
                            [ Html.text "Continue with custom Dropbox storage" ]
                ]

        CustomDropboxClientId ->
            Html.div []
                [ Html.input
                    [ placeholder "Dropbox client id"
                    , defaultValue model.dropboxClientId
                    , onInput ChangeDropboxClientId
                    ]
                    []
                , Html.text "TODO: instructions"
                ]
