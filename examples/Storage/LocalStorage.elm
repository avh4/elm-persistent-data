module Storage.LocalStorage exposing (storage)

{-| An example `Storage` implementation using HTTP.

@docs storage

-}

import PersistentCache
import Task
import Storage exposing (Storage)
import Storage.Hash as Hash
import Json.Decode
import Json.Encode


cache : PersistentCache.Cache String
cache =
    PersistentCache.cache
        { name = "Storage.LocalStorage"
        , version = 1
        , kilobytes = 2000
        , decode = Json.Decode.string
        , encode = Json.Encode.string
        }


{-| An example `Storage` implementation using HTTP.
-}
storage : Storage
storage =
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
                PersistentCache.get cache (Hash.toString hash)
        , write =
            \content ->
                let
                    hash =
                        Hash.ofString content
                in
                    PersistentCache.add cache (Hash.toString hash) content
                        |> Task.map (always hash)
        }
    }
