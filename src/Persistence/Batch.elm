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


encoder : (event -> Json.Encode.Value) -> Batch event -> Json.Encode.Value
encoder encodeEvent { events, parent } =
    [ ( "events"
      , List.map encodeEvent events
            |> Json.Encode.list
      )
    , ( "parent"
      , parent
            |> Maybe.map Hash.encode
            |> Maybe.withDefault Json.Encode.null
      )
    ]
        |> Json.Encode.object
