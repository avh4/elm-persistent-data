module Persistence.Batch exposing (Batch, decoder, encoder)

import Json.Decode exposing (Decoder)
import Json.Encode


type alias Batch event =
    { events : List event
    , parent : Maybe String
    }


decoder : Decoder event -> Decoder (Batch event)
decoder decodeEvent =
    Json.Decode.map2 Batch
        (Json.Decode.field "events" <| Json.Decode.list decodeEvent)
        (Json.Decode.field "parent" <| Json.Decode.nullable Json.Decode.string)


encoder : (event -> Json.Encode.Value) -> Batch event -> Json.Encode.Value
encoder encodeEvent { events, parent } =
    [ ( "events"
      , List.map encodeEvent events
            |> Json.Encode.list
      )
    , ( "parent"
      , parent
            |> Maybe.map Json.Encode.string
            |> Maybe.withDefault Json.Encode.null
      )
    ]
        |> Json.Encode.object
