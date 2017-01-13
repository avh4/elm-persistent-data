module Tests exposing (..)

import Test exposing (..)
import PersistenceTests
import Storage.CacheTests


all : Test
all =
    describe "elm-persisent-data"
        [ PersistenceTests.all
        , Storage.CacheTests.all
        ]
