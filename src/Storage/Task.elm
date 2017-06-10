module Storage.Task exposing (Config, storage)

{-| Creates a `Storage` implementation using the provided `Task`s.

@docs storage, Config

-}

import Storage exposing (Storage)
import Storage.Hash as Hash
import Task exposing (Task)


type alias Config =
    { get : String -> Task Never (Maybe String)
    , add : String -> String -> Task Never ()
    }


{-| A `Storage` implementation using the provided `Task`s.
-}
storage : Config -> Storage
storage config =
    { refs =
        { read =
            \key ->
                Task.fail "Not implemented"
        , write =
            \key oldValue newValue ->
                Task.fail "Not implemented"
        }
    , content =
        { read =
            \hash ->
                config.get (Hash.toString hash)
                    |> Task.mapError never
                    |> Task.andThen (Maybe.map Task.succeed >> Maybe.withDefault (Task.fail "Not found"))
        , write =
            \content ->
                let
                    hash =
                        Hash.ofString content
                in
                config.add (Hash.toString hash) content
                    |> Task.mapError never
                    |> Task.map (always hash)
        }
    }
