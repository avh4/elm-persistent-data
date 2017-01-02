module Persistence.LowLevelTests exposing (..)

import Test exposing (..)
import Expect
import Persistence.LowLevel as LowLevel


type alias Context =
    ()


start : Context
start =
    ()


resolveRead : String -> Result String (Maybe String) -> Context -> Context
resolveRead key result context =
    context


expectReplayedMessages : List msg -> Context -> Expect.Expectation
expectReplayedMessages expected context =
    Expect.fail "TODO: expectReplayedMessages not implmented yet"


expectError : String -> Context -> Expect.Expectation
expectError expectedError context =
    Expect.fail "TODO: expectError not implmented yet"


all : Test
all =
    describe "Persistence.LowLevel"
        [ describe "read"
            [ test "with no data" <|
                \() ->
                    start
                        |> resolveRead "root-v1" (Ok Nothing)
                        |> expectReplayedMessages []
            , test "with invalid data" <|
                \() ->
                    start
                        |> resolveRead "root-v1" (Ok <| Just ";;;not JSON")
                        |> expectError "Unable to parse persisted JSON"
            ]
        ]
