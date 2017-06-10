module Storage.LocalStorage exposing (storage)

{-| An example `Storage` implementation using HTTP.

@docs storage

-}

import Json.Decode
import Json.Encode
import PersistentCache
import Storage exposing (Storage)
import Storage.Hash as Hash
import Storage.Task
import Task


cache : PersistentCache.Cache String
cache =
    PersistentCache.cache
        { name = "Storage.LocalStorage"
        , version = 1
        , kilobytes = 2000
        , decode = Json.Decode.string
        , encode = Json.Encode.string
        }


{-| An example `Storage` implementation using Local Storage.
-}
storage : Storage
storage =
    Storage.Task.storage
        { get = PersistentCache.get cache
        , add = PersistentCache.add cache
        }
