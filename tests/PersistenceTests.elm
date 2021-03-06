module PersistenceTests exposing (all)

import Expect exposing (Expectation)
import Persistence
import Storage.Hash as Hash exposing (Hash)
import Test exposing (..)
import TestApp
import TestContextWithMocks as TestContext exposing (MockTask, TestContext)


type alias Mocks =
    { readRef : String -> MockTask String (Maybe String)
    , readContent : Hash -> MockTask String String
    , writeContent : String -> MockTask String Hash
    , writeRef : String -> Maybe String -> String -> MockTask String ()
    }


mocks : Mocks
mocks =
    { readRef =
        \key ->
            TestContext.mockTask ("refs.read:" ++ key)
    , writeRef =
        \key old new ->
            TestContext.mockTask ("refs.write:" ++ key ++ ":" ++ toString old ++ ":" ++ toString new)
    , readContent =
        \key ->
            TestContext.mockTask ("content.read:" ++ toString key)
    , writeContent =
        \content ->
            TestContext.mockTask ("content.write:" ++ content)
    }


start : TestContext (Persistence.Model TestApp.Data TestApp.UiState) (Persistence.Msg TestApp.Data TestApp.Event TestApp.Msg)
start =
    TestApp.program
        { refs =
            { read =
                mocks.readRef >> TestContext.toTask
            , write =
                \a b c ->
                    TestContext.toTask (mocks.writeRef a b c)
            }
        , content =
            { read = mocks.readContent >> TestContext.toTask
            , write =
                mocks.writeContent >> TestContext.toTask
            }
        }
        Nothing
        |> TestContext.start


hash : String -> Hash
hash =
    Hash.ofString


resolve :
    MockTask error a
    -> a
    -> TestContext model msg
    -> TestContext model msg
resolve mock value =
    TestContext.resolveMockTask mock (Ok value)


updateUi :
    msg
    -> TestContext model (Persistence.Msg data event msg)
    -> TestContext model (Persistence.Msg data event msg)
updateUi uiMsg =
    TestContext.update (uiMsg |> Persistence.uimsg)


expectOk : (a -> Expectation) -> Result String a -> Expectation
expectOk expectation result =
    case result of
        Err x ->
            Expect.fail x

        Ok a ->
            expectation a


testResults : a -> List (a -> Result String a) -> (a -> Expectation) -> Expectation
testResults init steps expect =
    List.foldl (\f a -> Result.andThen f a) (Ok init) steps
        |> expectOk expect


foldResults : List (a -> Result x a) -> a -> Result x a
foldResults steps init =
    List.foldl (\f a -> Result.andThen f a) (Ok init) steps


expectMockTask :
    MockTask x a
    -> TestContext model msg
    -> Expectation
expectMockTask =
    TestContext.expectMockTask


expectCurrent :
    Persistence.PersistenceState data ui
    -> TestContext (Persistence.Model data ui) msg
    -> Expectation
expectCurrent expected =
    TestContext.expectModel
        (Persistence.current
            >> Expect.equal expected
        )


all : Test
all =
    describe "Persistence"
        [ test "initially shows loading view" <|
            \() ->
                start
                    |> TestContext.expectModel
                        (Persistence.current
                            >> Expect.equal Persistence.Loading
                        )
        , describe "initial loading"
            [ test "with no previous data, when load succeeds, shows initial state" <|
                \() ->
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                        |> expectCurrent
                            (Persistence.Ready
                                { list = [] }
                                { input = "" }
                            )
            , test "with previous data in a single batch, when load succeeds, shows previous data" <|
                \() ->
                    let
                        batch =
                            """{"events":[{"tag":"AddItem","$0":"hello"}],"parent":null}"""
                    in
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| Hash.toString <| hash batch)
                        |> resolve (mocks.readContent (hash batch)) batch
                        |> expectCurrent
                            (Persistence.Ready
                                { list = [ "hello" ] }
                                { input = "" }
                            )
            , test "when batch contains multiple events, they replay in the correct order" <|
                \() ->
                    let
                        batch =
                            """{"events":[{"tag":"AddItem","$0":"hello"},{"tag":"AddItem","$0":"world"}],"parent":null}"""
                    in
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| Hash.toString <| hash batch)
                        |> resolve (mocks.readContent (hash batch)) batch
                        |> expectCurrent
                            (Persistence.Ready
                                { list = [ "world", "hello" ] }
                                { input = "" }
                            )
            , test "when loading the root fails, retry after 5 seconds" <|
                \() ->
                    start
                        |> TestContext.resolveMockTask
                            (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1")
                            (Err "Network error")
                        |> TestContext.advanceTime 5000
                        |> expectMockTask (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1")

            -- TODO: error loading root
            -- TODO: error loading batch
            -- TODO: batch doesn't exist (fatal error)
            -- TODO: error decoding batch JSON
            , test "with previous data in multiple batches, when load succeeds, shows previous data" <|
                \() ->
                    let
                        batch1 =
                            """{"events":[{"tag":"AddItem","$0":"hello"}],"parent":null}"""

                        batch2 =
                            """{"events":[{"tag":"AddItem","$0":"world"}],"parent":""" ++ "\"" ++ Hash.toString (hash batch1) ++ """"}"""
                    in
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| Hash.toString <| hash batch2)
                        |> resolve (mocks.readContent (hash batch2)) batch2
                        |> resolve (mocks.readContent (hash batch1)) batch1
                        -- TODO: Implement low-memory scanning in Persistence.Initializer
                        -- |> resolve (mocks.readContent (hash batch2)) batch2
                        |> expectCurrent
                            (Persistence.Ready
                                { list = [ "world", "hello" ] }
                                { input = "" }
                            )

            -- TODO: should be in Loading state until all batches finish
            -- TODO: error loading second batch fails
            -- TODO: someday: with cached reduction
            ]
        , describe "update"
            [ test "UI messages should update the UI state" <|
                \() ->
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                        |> updateUi (TestApp.Typed "world")
                        |> expectCurrent
                            (Persistence.Ready
                                { list = [] }
                                { input = "world" }
                            )
            , test "when an event is produced, it updates the app data" <|
                \() ->
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                        |> updateUi (TestApp.Typed "world")
                        |> updateUi TestApp.Add
                        |> expectCurrent
                            (Persistence.Ready
                                { list = [ "world" ] }
                                { input = "" }
                            )
            ]
        , describe "writing a new event"
            [ test "initiates the write" <|
                \() ->
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                        |> updateUi (TestApp.Typed "world")
                        |> updateUi TestApp.Add
                        |> expectMockTask
                            (mocks.writeContent """{"events":[{"tag":"AddItem","$0":"world"}],"parent":null}""")

            -- TODO: when the batch fails
            , test "when the batch succeeds, writes root" <|
                \() ->
                    let
                        batch =
                            """{"events":[{"tag":"AddItem","$0":"world"}],"parent":null}"""
                    in
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                        |> updateUi (TestApp.Typed "world")
                        |> updateUi TestApp.Add
                        |> resolve (mocks.writeContent batch) (hash batch)
                        |> expectMockTask
                            (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" Nothing (Hash.toString <| hash batch))
            , test "with a previous root, sets the parent" <|
                \() ->
                    let
                        batch1 =
                            """{"events":[],"parent":null}"""

                        batch2 =
                            """{"events":[{"tag":"AddItem","$0":"world"}],"parent":""" ++ "\"" ++ Hash.toString (hash batch1) ++ """"}"""
                    in
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| Hash.toString <| hash batch1)
                        |> resolve (mocks.readContent (hash batch1)) batch1
                        |> updateUi (TestApp.Typed "world")
                        |> updateUi TestApp.Add
                        |> resolve (mocks.writeContent batch2) (hash batch2)
                        |> expectMockTask
                            (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" (Just <| Hash.toString <| hash batch1) (Hash.toString <| hash batch2))
            , test "with a previous root and update, sets the parent" <|
                \() ->
                    let
                        batch1 =
                            """{"events":[],"parent":null}"""

                        batch2 =
                            """{"events":[{"tag":"AddItem","$0":"buy carrots"}],"parent":""" ++ "\"" ++ Hash.toString (hash batch1) ++ """"}"""

                        batch3 =
                            """{"events":[{"tag":"AddItem","$0":"check cookies"}],"parent":""" ++ "\"" ++ Hash.toString (hash batch2) ++ """"}"""
                    in
                    start
                        |> resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| Hash.toString <| hash batch1)
                        |> resolve (mocks.readContent (hash batch1)) batch1
                        |> updateUi (TestApp.Typed "buy carrots")
                        |> updateUi TestApp.Add
                        |> resolve (mocks.writeContent batch2) (hash batch2)
                        |> resolve (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" (Just <| Hash.toString <| hash batch1) (Hash.toString <| hash batch2)) ()
                        |> updateUi (TestApp.Typed "check cookies")
                        |> updateUi TestApp.Add
                        |> resolve (mocks.writeContent batch3) (hash batch3)
                        |> expectMockTask
                            (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" (Just <| Hash.toString <| hash batch2) (Hash.toString <| hash batch3))

            -- TODO: a new event happens before writing finishes
            ]
        ]
