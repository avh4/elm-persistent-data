module Storage.Hash exposing (Hash(..), encode, decode, fromString, toString, ofString)

{-| This is the interface that must be implemented to connect a storage
implementaiton to `Persistence`.

@docs Hash, decode, encode, fromString, toString, ofString

-}

import Json.Decode
import Json.Encode
import Sha256


{-| A key for the content store, which is the SHA-256 hash of the value.
-}
type Hash
    = Sha256 String


{-| Decode a `Hash`
-}
decode : Json.Decode.Decoder Hash
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case fromString string of
                    Ok hash ->
                        Json.Decode.succeed hash

                    Err message ->
                        Json.Decode.fail message
            )


{-| Encode a `Hash`
-}
encode : Hash -> Json.Encode.Value
encode =
    toString
        >> Json.Encode.string


{-| Parse a `Hash`
-}
fromString : String -> Result String Hash
fromString string =
    case String.split "-" string of
        [ "sha256", hash ] ->
            Ok (Sha256 hash)

        _ ->
            Err ("Unexpected hash: " ++ string)


{-| Convert a `Hash` to a String
-}
toString : Hash -> String
toString (Sha256 hash) =
    ("sha256-" ++ hash)


{-| Calculate the `Hash` of a String.
-}
ofString : String -> Hash
ofString string =
    Sha256 (Sha256.sha256 string)
