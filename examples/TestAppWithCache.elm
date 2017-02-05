module TestAppWithCache exposing (main)

import TestApp
import Storage.Debug
import Storage.Cache
import Storage.LocalStorage
import Storage.ExampleServer
import PersistentCache
import Json.Decode
import Json.Encode


main =
    TestApp.program
        (Storage.Cache.cache
            (Storage.Debug.storage "local storage" Storage.LocalStorage.storage)
            (Storage.Debug.storage "example-server (HTTP)" <| Storage.ExampleServer.storage "/")
        )
        (Just
            { read = PersistentCache.get cache ".data"
            , write = PersistentCache.add cache ".data"
            }
        )


cache : PersistentCache.Cache String
cache =
    PersistentCache.cache
        { name = "Storage.LocalStorage"
        , version = 1
        , kilobytes = 2000
        , decode = Json.Decode.string
        , encode = Json.Encode.string
        }
