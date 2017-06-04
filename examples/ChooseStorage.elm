module ChooseStorage exposing (programRecord)

import Debug.Control as Control exposing (Control)
import Dropbox
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import PersistentCache
import ProgramWithAuth
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
    | Dropbox Dropbox.UserAuth
    | LocalStorage


createStorage : Config -> ( Storage, Maybe Storage.CacheStore )
createStorage config =
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
                    debug "Dropbox" <| Storage.Dropbox.storage userAuth

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
                              , Control.map Dropbox.authorizationFromAccessToken <|
                                    Control.string ""
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


update : Msg -> Model -> Result ( Model, Cmd Msg ) ( Storage, Maybe Storage.CacheStore )
update msg model =
    case msg of
        ChangeConfig model ->
            Err ( model, Cmd.none )

        Done ->
            Ok (createStorage <| Control.currentValue model)


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


programRecord : ProgramWithAuth.AuthProgram Never ( Storage, Maybe Storage.CacheStore ) Model Msg
programRecord =
    { init = ProgramWithAuth.NoArgs (Err ( initialModel, Cmd.none ))
    , subscriptions = \_ -> Sub.none
    , update = update
    , view = view
    }
