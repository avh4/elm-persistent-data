module Tests exposing (..)

import Test exposing (..)
import Persistence.LowLevelTests


all : Test
all =
    describe "elm-persisent-data"
        [ Persistence.LowLevelTests.all
        ]
