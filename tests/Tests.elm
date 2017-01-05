module Tests exposing (..)

import Test exposing (..)
import PersistenceTests


all : Test
all =
    describe "elm-persisent-data"
        [ PersistenceTests.all
        ]
