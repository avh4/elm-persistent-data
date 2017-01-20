module PersistenceTests exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)
import TestContextWithMocks as TestContext exposing (TestContext, MockTask)
import TestApp
import Persistence
import Storage.Hash as Hash exposing (Hash)


type alias Mocks =
    { readRef : String -> MockTask String (Maybe Hash)
    , readContent : Hash -> MockTask String (Maybe String)
    , writeContent : String -> MockTask String Hash
    , writeRef : String -> Maybe Hash -> Hash -> MockTask String ()
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


start : TestContext (Persistence.Model TestApp.Data TestApp.UiState) (Persistence.Msg TestApp.Event TestApp.Msg)
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
        |> TestContext.start


hash : String -> Hash
hash =
    Hash.ofString


resolve :
    MockTask error a
    -> a
    -> TestContext model msg
    -> Result String (TestContext model msg)
resolve mock value =
    TestContext.resolveMockTask mock (Ok value)


updateUi :
    msg
    -> TestContext model (Persistence.Msg event msg)
    -> Result error (TestContext model (Persistence.Msg event msg))
updateUi uiMsg =
    TestContext.update (uiMsg |> Persistence.uimsg) >> Ok


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
    -> Result String (TestContext model msg)
    -> Expectation
expectMockTask =
    TestContext.expectMockTask >> expectOk


expectCurrent :
    Persistence.PersistenceState data ui
    -> Result String (TestContext (Persistence.Model data ui) msg)
    -> Expectation
expectCurrent expected =
    expectOk
        (TestContext.model
            >> Persistence.current
            >> Expect.equal expected
        )


all : Test
all =
    describe "Persistence"
        [ test "initially shows loading view" <|
            \() ->
                start
                    |> TestContext.model
                    |> Persistence.current
                    |> Expect.equal Persistence.Loading
        , describe "initial loading"
            [ test "with no previous data, when load succeeds, shows initial state" <|
                \() ->
                    testResults start
                        [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing ]
                        (TestContext.model
                            >> Persistence.current
                            >> Expect.equal
                                (Persistence.Ready
                                    { list = [] }
                                    { input = "" }
                                )
                        )
            , test "with previous data in a single batch, when load succeeds, shows previous data" <|
                \() ->
                    let
                        batch =
                            """{"events":[{"tag":"AddItem","$0":"hello"}],"parent":null}"""
                    in
                        testResults start
                            [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| hash batch)
                            , resolve (mocks.readContent (hash batch)) (Just batch)
                            ]
                            (TestContext.model
                                >> Persistence.current
                                >> Expect.equal
                                    (Persistence.Ready
                                        { list = [ "hello" ] }
                                        { input = "" }
                                    )
                            )
              -- TODO: verify ordering of event replay
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
                        testResults start
                            [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| hash batch2)
                            , resolve (mocks.readContent (hash batch2)) (Just batch2)
                            , resolve (mocks.readContent (hash batch1)) (Just batch1)
                            ]
                            (TestContext.model
                                >> Persistence.current
                                >> Expect.equal
                                    (Persistence.Ready
                                        { list = [ "world", "hello" ] }
                                        { input = "" }
                                    )
                            )
              -- TODO: should be in Loading state until all batches finish
              -- TODO: error loading second batch fails
              -- TODO: someday: with cached reduction
            ]
        , describe "update"
            [ test "UI messages should update the UI state" <|
                \() ->
                    testResults
                        start
                        [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                        , updateUi (TestApp.Typed "world")
                        ]
                        (TestContext.model
                            >> Persistence.current
                            >> Expect.equal
                                (Persistence.Ready
                                    { list = [] }
                                    { input = "world" }
                                )
                        )
            , test "when an event is produced, it updates the app data" <|
                \() ->
                    start
                        |> foldResults
                            [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            ]
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
                        |> foldResults
                            [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            ]
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
                            |> foldResults
                                [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") Nothing
                                , updateUi (TestApp.Typed "world")
                                , updateUi (TestApp.Add)
                                , resolve (mocks.writeContent batch) (hash batch)
                                ]
                            |> expectMockTask
                                (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" Nothing (hash batch))
            , test "with a previous root, sets the parent" <|
                \() ->
                    let
                        batch1 =
                            """{}"""

                        batch2 =
                            """{"events":[{"tag":"AddItem","$0":"world"}],"parent":""" ++ "\"" ++ Hash.toString (hash batch1) ++ """"}"""
                    in
                        start
                            |> foldResults
                                [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| hash batch1)
                                , resolve (mocks.readContent (hash batch1)) (Just batch1)
                                , updateUi (TestApp.Typed "world")
                                , updateUi (TestApp.Add)
                                , resolve (mocks.writeContent batch2) (hash batch2)
                                ]
                            |> expectMockTask
                                (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" (Just <| hash batch1) (hash batch2))
            , test "with a previous root, sets the parent" <|
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
                            |> foldResults
                                [ resolve (mocks.readRef "io.github.avh4.elm-persistent-data.test-app.root-v1") (Just <| hash batch1)
                                , resolve (mocks.readContent (hash batch1)) (Just batch1)
                                , updateUi (TestApp.Typed "buy carrots")
                                , updateUi (TestApp.Add)
                                , resolve (mocks.writeContent batch2) (hash batch2)
                                , resolve (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" (Just <| hash batch1) (hash batch2)) ()
                                , updateUi (TestApp.Typed "check cookies")
                                , updateUi (TestApp.Add)
                                , resolve (mocks.writeContent batch3) (hash batch3)
                                ]
                            |> expectMockTask
                                (mocks.writeRef "io.github.avh4.elm-persistent-data.test-app.root-v1" (Just <| hash batch2) (hash batch3))
              -- TODO: a new event happens before writing finishes
            ]
        ]
