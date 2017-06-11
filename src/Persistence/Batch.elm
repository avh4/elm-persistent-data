module Persistence.Batch exposing (Batch, decoder, encoder)

import Json.Decode exposing (Decoder)
import Json.Encode
import Storage.Hash as Hash exposing (Hash)


type alias Batch event =
    { events : List event
    , parent : Maybe Hash
    }


decoder : Decoder event -> Decoder (Batch event)
decoder decodeEvent =
    Json.Decode.map2 Batch
        (Json.Decode.field "events" <| Json.Decode.list decodeEvent)
        (Json.Decode.field "parent" <| Json.Decode.nullable Hash.decode)


{-| This also verifies that the events can be decoded, and fails if they cannot.
-}
encoder : Decoder event -> (event -> Json.Encode.Value) -> Batch event -> Result String Json.Encode.Value
encoder decodeEvent encodeEvent { events, parent } =
    let
        verifiedEvents =
            List.foldr (\event -> Result.andThen (verifyEvent event)) (Ok []) events

        verifyEvent event acc =
            let
                json =
                    encodeEvent event

                decoded =
                    Json.Decode.decodeValue decodeEvent json
            in
            if Ok event /= decoded then
                Err
                    ("Encoded value does not decode:\n\nOriginal:\n  "
                        ++ toString event
                        ++ "\nJSON:\n  "
                        ++ Json.Encode.encode 0 json
                        ++ "\nDecoded:\n  "
                        ++ toString decoded
                    )
            else
                Ok (json :: acc)
    in
    case verifiedEvents of
        Err message ->
            Err message

        Ok events ->
            Json.Encode.object
                [ ( "events", Json.Encode.list events )
                , ( "parent"
                  , parent
                        |> Maybe.map Hash.encode
                        |> Maybe.withDefault Json.Encode.null
                  )
                ]
                |> Ok
