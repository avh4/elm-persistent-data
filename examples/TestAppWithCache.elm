module TestAppWithCache exposing (main)

import TestApp
import Storage.Debug
import Storage.Cache
import Storage.LocalStorage
import Storage.ExampleServer


main =
    TestApp.program
        (Storage.Cache.cache
            (Storage.Debug.storage "local storage" Storage.LocalStorage.storage)
            (Storage.Debug.storage "example-server (HTTP)" Storage.ExampleServer.storage)
        )
