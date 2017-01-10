module PersistenceTests exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)
import TestContextWithMocks as TestContext exposing (TestContext, MockTask)
import TestApp
import Persistence
import Sha256


type alias Mocks =
    { read : String -> MockTask String (Maybe String)
    , writeContent : String -> MockTask String String
    , writeRef : String -> Maybe String -> String -> MockTask String ()
    }


start : TestContext Mocks (Persistence.Model TestApp.Data TestApp.UiState) (Persistence.Msg TestApp.Event TestApp.Msg)
start =
    TestContext.start
        (\mocks ->
            TestApp.program
                { read =
                    mocks.read >> TestContext.toTask
                , writeContent =
                    mocks.writeContent >> TestContext.toTask
                , writeRef =
                    \a b c ->
                        TestContext.toTask (mocks.writeRef a b c)
                }
        )
        (\token ->
            { read =
                \key ->
                    TestContext.mockTask token ("read:" ++ key)
            , writeContent =
                \content ->
                    TestContext.mockTask token ("writeContent:" ++ content)
            , writeRef =
                \key old new ->
                    TestContext.mockTask token ("writeRef:" ++ key ++ ":" ++ toString old ++ ":" ++ new)
            }
        )


resolveRead :
    String
    -> Maybe String
    -> TestContext Mocks model msg
    -> Result String (TestContext Mocks model msg)
resolveRead key value =
    TestContext.resolveMockTask (.read >> (|>) key) (Ok value)


resolveWrite :
    String
    -> TestContext Mocks model msg
    -> Result String (TestContext Mocks model msg)
resolveWrite content =
    let
        key =
            "sha256-" ++ Sha256.sha256 content
    in
        TestContext.resolveMockTask
            (.writeContent >> (|>) content)
            (Ok key)


resolveWriteRef :
    String
    -> Maybe String
    -> String
    -> TestContext Mocks model msg
    -> Result String (TestContext Mocks model msg)
resolveWriteRef key oldValue newValue =
    TestContext.resolveMockTask
        (\mocks -> mocks.writeRef key oldValue newValue)
        (Ok ())


updateUi :
    msg
    -> TestContext mocks model (Persistence.Msg event msg)
    -> Result error (TestContext mocks model (Persistence.Msg event msg))
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
    (mocks -> MockTask x a)
    -> Result String (TestContext mocks model msg)
    -> Expectation
expectMockTask =
    TestContext.expectMockTask >> expectOk


expectCurrent :
    Persistence.PersistenceState data ui
    -> Result String (TestContext mocks (Persistence.Model data ui) msg)
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
                        [ resolveRead "root-v1" Nothing ]
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
                    testResults start
                        [ resolveRead "root-v1" (Just "batch1")
                        , resolveRead "batch1" (Just """{"events":[{"tag":"AddItem","$0":"hello"}],"parent":null}""")
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
                    testResults start
                        [ resolveRead "root-v1" (Just "batch2")
                        , resolveRead "batch2" (Just """{"events":[{"tag":"AddItem","$0":"world"}],"parent":"batch1"}""")
                        , resolveRead "batch1" (Just """{"events":[{"tag":"AddItem","$0":"hello"}],"parent":null}""")
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
                        [ resolveRead "root-v1" Nothing
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
                            [ resolveRead "root-v1" Nothing
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
                            [ resolveRead "root-v1" Nothing
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            ]
                        |> expectMockTask
                            (.writeContent >> (|>) """{"events":[{"tag":"AddItem","$0":"world"}],"parent":null}""")
              -- TODO: when the batch fails
            , test "when the batch succeeds, writes root" <|
                \() ->
                    start
                        |> foldResults
                            [ resolveRead "root-v1" Nothing
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            , resolveWrite """{"events":[{"tag":"AddItem","$0":"world"}],"parent":null}"""
                            ]
                        |> expectMockTask
                            (.writeRef >> (\f -> f "root-v1" Nothing "sha256-fac6de7c836658da9d006a860e5affefe8e0fc2722c6a2326616a39722db0d37"))
            , test "with a previous root, sets the parent" <|
                \() ->
                    start
                        |> foldResults
                            [ resolveRead "root-v1" (Just "batch1")
                            , resolveRead "batch1" (Just """{}""")
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            , resolveWrite """{"events":[{"tag":"AddItem","$0":"world"}],"parent":"batch1"}"""
                            ]
                        |> expectMockTask
                            (.writeRef >> (\f -> f "root-v1" (Just "batch1") "sha256-2a15be4b4207fb34dea2cbfc3ec77d655441bcb4cba0d1da83d1e020a94fa397"))
            , test "with a previous root, sets the parent" <|
                \() ->
                    start
                        |> foldResults
                            [ resolveRead "root-v1" (Just "batch1")
                            , resolveRead "batch1" (Just """{"events":[],"parent":null}""")
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            , resolveWrite """{"events":[{"tag":"AddItem","$0":"world"}],"parent":"batch1"}"""
                            , resolveWriteRef
                                "root-v1"
                                (Just "batch1")
                                "sha256-2a15be4b4207fb34dea2cbfc3ec77d655441bcb4cba0d1da83d1e020a94fa397"
                            , updateUi (TestApp.Typed "again")
                            , updateUi (TestApp.Add)
                            , resolveWrite
                                """{"events":[{"tag":"AddItem","$0":"again"}],"parent":"sha256-2a15be4b4207fb34dea2cbfc3ec77d655441bcb4cba0d1da83d1e020a94fa397"}"""
                            ]
                        |> expectMockTask
                            (.writeRef >> (\f -> f "root-v1" (Just "sha256-2a15be4b4207fb34dea2cbfc3ec77d655441bcb4cba0d1da83d1e020a94fa397") "sha256-8748a9e85c444680b1ab14c94bf6aebcc433f71c836132a0e8a3a52ab71abf2a"))
              -- TODO: a new event happens before writing finishes
            ]
        ]
