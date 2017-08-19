module Persistence.SimpleAuth exposing (Config, Model, Msg, init, update, view)

import Dropbox
import Html exposing (Html)
import Html.Attributes exposing (defaultValue, placeholder)
import Html.Events exposing (onClick, onInput)
import Navigation
import Persistence.SimpleStorageConfig as StorageConfig exposing (StorageConfig)


type alias Config =
    { dropboxAppKey : Maybe String
    }


type Model
    = Model
        { location : Navigation.Location
        , step : Step
        , config : Config
        , dropboxAuthToken : String
        , dropboxClientId : String
        , flashMessages : List String
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


init : Config -> Navigation.Location -> Result ( Model, Cmd Msg ) ( StorageConfig, Cmd never )
init config location =
    let
        model =
            { location = location
            , step = Choose
            , config = config
            , dropboxAuthToken = ""
            , dropboxClientId = ""
            , flashMessages = []
            }

        dropboxResult =
            Dropbox.parseAuthorizeResult location

        clearDropboxRedirectResult =
            -- TODO: preserve other fragment data if there was any (is that even possible after a Dropbox redirect?)
            -- TODO: don't leave the unnecessary "#" at the end of the URL
            Navigation.modifyUrl "#"
    in
    case dropboxResult of
        Just (Dropbox.AuthorizeOk { userAuth }) ->
            Ok
                ( StorageConfig.Dropbox userAuth
                , clearDropboxRedirectResult
                )

        Just (Dropbox.DropboxAuthorizeErr err) ->
            Err
                ( Model
                    { model
                        | step = Choose
                        , flashMessages =
                            [ "Dropbox authentication failed: " ++ err.errorDescription ]
                    }
                , clearDropboxRedirectResult
                )

        Just (Dropbox.UnknownAccessTokenErr _) ->
            Err
                ( Model
                    { model
                        | step = Choose
                        , flashMessages =
                            [ "Dropbox authentication failed because Dropbox provided invalid data.  This shouldn't normally happen." ]
                    }
                , clearDropboxRedirectResult
                )

        Nothing ->
            Err
                ( Model model
                , Cmd.none
                )


update : Msg -> Model -> Result ( Model, Cmd Msg ) ( StorageConfig, Cmd never )
update msg (Model model) =
    let
        continue newModel =
            Err ( Model newModel, Cmd.none )
    in
    case ( model.step, msg ) of
        ( _, ChooseDropbox clientId ) ->
            Err
                ( Model model
                  -- Dropbox.authorize will redirect, so no need to change the model
                , Dropbox.authorize
                    { clientId = clientId
                    , state = Nothing
                    , requireRole = Nothing
                    , forceReapprove = False
                    , disableSignup = False
                    , locale = Nothing
                    , forceReauthentication = False
                    }
                    model.location
                )

        ( _, ChooseChooseCustom ) ->
            continue { model | step = ChooseCustom }

        ( _, ChooseCustomDropboxAuthToken ) ->
            continue { model | step = CustomDropboxAuthToken }

        ( _, ChooseCustomDropboxClientId ) ->
            continue { model | step = CustomDropboxClientId }

        ( _, ChangeDropboxAuthToken new ) ->
            continue { model | dropboxAuthToken = new }

        ( _, CompleteCustomDropboxAuthToken authToken ) ->
            Ok
                ( StorageConfig.Dropbox <| Dropbox.authorizationFromAccessToken authToken
                , Cmd.none
                )

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
                [ -- TODO: maybe move flash message outside of Choose?
                  model.flashMessages
                    |> List.map (\m -> Html.li [] [ Html.text m ])
                    |> Html.ul []
                , case model.config.dropboxAppKey of
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
