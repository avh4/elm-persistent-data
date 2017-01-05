module Persistence.LowLevel exposing (EventId, read, write)

import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)
import NonEmptyList exposing (NonEmptyList)


type alias Config msg =
    { decoder : Decoder msg
    , encoder : Encoder msg
    , read : String -> Task String (Maybe String)
    , write : String -> String -> Task String ()
    }


type EventId
    = Root
    | Child Int


type alias Encoder a =
    a -> Json.Encode.Value


read : Config msg -> EventId -> Task String ( EventId, List msg )
read config lastKnownEventId =
    Task.succeed ( Root, [] )


write :
    Config msg
    -> ( EventId, List msg )
    -> Task String (Result ( EventId, NonEmptyList msg ) EventId)
write config ( lastKnownEventId, newEvents ) =
    Task.succeed (Ok Root)
