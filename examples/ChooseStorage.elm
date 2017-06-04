module ChooseStorage exposing (create, decoder, encoder, programRecord)

import Debug.Control as Control exposing (Control)
import Dropbox
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import PersistentCache
import ProgramRecord exposing (ProgramRecord)
import Storage exposing (Storage)
import Storage.Cache
import Storage.Debug
import Storage.Dropbox
import Storage.ExampleServer
import Storage.LocalStorage


type alias Config =
    { storage : StorageType
    , debug : Bool
    , cacheStorage : Bool
    , cacheData : Bool
    }


type StorageType
    = ExampleServer String
    | Dropbox String
    | LocalStorage


encoder : Config -> Json.Encode.Value
encoder config =
    Json.Encode.object
        [ ( "storage", encodeStorageType config.storage )
        , ( "debug", Json.Encode.bool config.debug )
        , ( "cacheStorage", Json.Encode.bool config.cacheStorage )
        , ( "cacheData", Json.Encode.bool config.cacheData )
        ]


encodeStorageType : StorageType -> Json.Encode.Value
encodeStorageType s =
    case s of
        ExampleServer rootUrl ->
            Json.Encode.object
                [ ( ".tag", Json.Encode.string "ExampleServer" )
                , ( ".0", Json.Encode.string rootUrl )
                ]

        Dropbox accessToken ->
            Json.Encode.object
                [ ( ".tag", Json.Encode.string "Dropbox" )
                , ( ".0", Json.Encode.string accessToken )
                ]

        LocalStorage ->
            Json.Encode.object
                [ ( ".tag", Json.Encode.string "LocalStorage" )
                ]


decoder : Json.Decode.Decoder Config
decoder =
    Json.Decode.map4 Config
        (Json.Decode.field "storage" decodeStorageType)
        (Json.Decode.field "debug" Json.Decode.bool)
        (Json.Decode.field "cacheStorage" Json.Decode.bool)
        (Json.Decode.field "cacheData" Json.Decode.bool)


decodeStorageType : Json.Decode.Decoder StorageType
decodeStorageType =
    Json.Decode.field ".tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                case tag of
                    "ExampleServer" ->
                        Json.Decode.map ExampleServer <| Json.Decode.field ".0" Json.Decode.string

                    "Dropbox" ->
                        Json.Decode.map Dropbox <| Json.Decode.field ".0" Json.Decode.string

                    "LocalStorage" ->
                        Json.Decode.succeed LocalStorage

                    _ ->
                        Json.Decode.fail ("Unknown .tag: " ++ tag)
            )


create : Config -> ( Storage, Maybe Storage.CacheStore )
create config =
    let
        debug name s =
            if config.debug then
                Storage.Debug.storage name s
            else
                s

        base =
            case config.storage of
                ExampleServer rootUrl ->
                    debug "example-server (HTTP)" <| Storage.ExampleServer.storage rootUrl

                Dropbox userAuth ->
                    debug "Dropbox" <| Storage.Dropbox.storage <| Dropbox.authorizationFromAccessToken userAuth

                LocalStorage ->
                    debug "local storage" <| Storage.LocalStorage.storage

        cached =
            if config.cacheStorage then
                Storage.Cache.cache
                    (debug "local storage cache" Storage.LocalStorage.storage)
                    base
            else
                base

        dataCache =
            if config.cacheData then
                let
                    cache =
                        PersistentCache.cache
                            { name = "Storage.LocalStorage"
                            , version = 1
                            , kilobytes = 2000
                            , decode = Json.Decode.string
                            , encode = Json.Encode.string
                            }
                in
                Just <|
                    (if config.debug then
                        Storage.Debug.cache "data cache"
                     else
                        identity
                    )
                        { read = PersistentCache.get cache ".data"
                        , write = PersistentCache.add cache ".data"
                        }
            else
                Nothing
    in
    ( cached, dataCache )


type alias Model =
    Control Config


initialModel : Model
initialModel =
    Control.record Config
        |> Control.field "storage backend"
            (Control.choice
                [ ( "Storage.ExampleServer"
                  , Control.record ExampleServer
                        |> Control.field "root URL" (Control.string "/")
                  )
                , ( "Storage.Dropbox"
                  , Control.map Dropbox <|
                        Control.choice
                            [ ( "Generated Auth Token"
                              , Control.string ""
                              )
                            ]
                  )
                , ( "Storage.LocalStorage", Control.value LocalStorage )
                ]
            )
        |> Control.field "log to js console" (Control.bool True)
        |> Control.field "cache storage requests" (Control.bool True)
        |> Control.field "cache computed data" (Control.bool True)


type Msg
    = ChangeConfig (Control Config)
    | Done


update : Msg -> Model -> Result ( Model, Cmd Msg ) Config
update msg model =
    case msg of
        ChangeConfig model ->
            Err ( model, Cmd.none )

        Done ->
            Ok (Control.currentValue model)


view : Model -> Html Msg
view model =
    div []
        [ Html.h2 [] [ text "Choose your storage type" ]
        , Control.view ChangeConfig model
        , button
            [ onClick Done ]
            [ text "Start app"
            ]
        ]


programRecord : ProgramRecord Never Config Model Msg
programRecord =
    ProgramRecord.completableProgram
        { init = Err ( initialModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
