module Tests exposing (..)

import DataCacheTests
import PersistenceTests
import Storage.CacheTests
import Test exposing (..)


all : Test
all =
    describe "elm-persisent-data"
        [ PersistenceTests.all
        , Storage.CacheTests.all
        , DataCacheTests.all
        ]
