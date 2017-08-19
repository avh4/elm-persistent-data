module Persistence.SimpleStorageConfig exposing (StorageConfig(..), decoder, encode, toStorage)

import Dropbox
import Json.Decode as Decode
import Json.Encode as Encode
import Storage exposing (Storage)
import Storage.Dropbox


type StorageConfig
    = Dropbox String


toStorage : StorageConfig -> Storage
toStorage config =
    case config of
        Dropbox authToken ->
            Storage.Dropbox.storage <|
                Dropbox.authorizationFromAccessToken authToken


decoder : Decode.Decoder StorageConfig
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "dropbox" ->
                        Decode.field "authToken" Decode.string
                            |> Decode.map Dropbox

                    _ ->
                        Decode.fail ("Got an unknown type: " ++ toString t)
            )


{-| WARNING: the resulting JSON will potentially contain authentication
credentials for your user.
Be careful with the resulting value
and handle it securely.
-}
encode : StorageConfig -> Encode.Value
encode config =
    case config of
        Dropbox authToken ->
            Encode.object
                [ ( "type", Encode.string "dropbox" )
                , ( "authToken", Encode.string authToken )
                ]
