module PersistenceTests exposing (all)

import Test exposing (..)
import Expect
import TestContext exposing (TestContext)
import TestApp
import Persistence
import Task
import Process


all : Test
all =
    describe "Persistence"
        [ test "initially shows loading view" <|
            \() ->
                TestApp.program
                    { read = \_ -> Task.map (always <| Nothing) <| Process.sleep 999 }
                    |> TestContext.start
                    |> TestContext.model
                    |> Persistence.current
                    |> Expect.equal Persistence.Loading
        , describe "initial loading"
            [ test "with no previous data, when load succeeds, shows initial state" <|
                \() ->
                    TestApp.program
                        { read = \_ -> Task.succeed Nothing }
                        |> TestContext.start
                        |> TestContext.model
                        |> Persistence.current
                        |> Expect.equal
                            (Persistence.Ready
                                { list = [] }
                                { input = "" }
                            )
            , test "with previous data in a single batch, when load succeeds, shows previous data" <|
                \() ->
                    TestApp.program
                        { read =
                            \key ->
                                case key of
                                    "root-v1" ->
                                        Task.succeed (Just "batch1")

                                    "batch1" ->
                                        Task.succeed (Just """{"events":[{"tag":"AddItem","$0":"hello"}]}""")

                                    _ ->
                                        Task.succeed Nothing
                        }
                        |> TestContext.start
                        |> TestContext.model
                        |> Persistence.current
                        |> Expect.equal
                            (Persistence.Ready
                                { list = [ "hello" ] }
                                { input = "" }
                            )
              -- TODO: verify ordering of event replay
              -- TODO: multiple initial batches
              -- TODO: someday: with cached reduction
              -- TODO: error loading root
              -- TODO: error loading batch
              -- TODO: batch doesn't exist (fatal error)
              -- TODO: error decoding batch JSON
            ]
        , describe "update"
            [ test "UI messages should update the UI state" <|
                \() ->
                    TestApp.program
                        { read = \_ -> Task.succeed Nothing }
                        |> TestContext.start
                        |> TestContext.update (TestApp.Typed "world" |> Persistence.uimsg)
                        |> TestContext.model
                        |> Persistence.current
                        |> Expect.equal
                            (Persistence.Ready
                                { list = [] }
                                { input = "world" }
                            )
            , test "when an event is produced, it updates the app data" <|
                \() ->
                    TestApp.program
                        { read = \_ -> Task.succeed Nothing }
                        |> TestContext.start
                        |> TestContext.update (TestApp.Typed "world" |> Persistence.uimsg)
                        |> TestContext.update (TestApp.Add |> Persistence.uimsg)
                        |> TestContext.model
                        |> Persistence.current
                        |> Expect.equal
                            (Persistence.Ready
                                { list = [ "world" ] }
                                { input = "" }
                            )
            ]
        ]
