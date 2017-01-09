module PersistenceTests exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)
import TestContextWithMocks as TestContext exposing (TestContext, MockTask)
import TestApp
import Persistence


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


updateUi :
    msg
    -> TestContext mocks model (Persistence.Msg event msg)
    -> Result error (TestContext mocks model (Persistence.Msg event msg))
updateUi uiMsg =
    TestContext.update (uiMsg |> Persistence.uimsg) >> Ok


expectOk : (a -> Expectation) -> Result x a -> Expectation
expectOk expectation result =
    case result of
        Err x ->
            [ toString result
            , "╷"
            , "│ expectOk"
            , "╵"
            , "Ok _"
            ]
                |> String.join "\n"
                |> Expect.fail

        Ok a ->
            expectation a


testResults : a -> List (a -> Result x a) -> (a -> Expectation) -> Expectation
testResults init steps expect =
    List.foldl (\f a -> Result.andThen f a) (Ok init) steps
        |> expectOk expect


foldResults : List (a -> Result x a) -> a -> Result x a
foldResults steps init =
    List.foldl (\f a -> Result.andThen f a) (Ok init) steps


expectMockTask :
    (mocks -> MockTask x a)
    -> Result x1 (TestContext mocks model msg)
    -> Expectation
expectMockTask =
    TestContext.expectMockTask >> expectOk


expectCurrent :
    Persistence.PersistenceState data ui
    -> Result x (TestContext mocks (Persistence.Model data ui) msg)
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
                        , resolveRead "batch1" (Just """{"events":[{"tag":"AddItem","$0":"hello"}]}""")
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
                            [ updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            ]
                        |> expectMockTask
                            -- TODO: include parent in JSON
                            (.writeContent >> (|>) "{\"events\":[{\"tag\":\"AddItem\",\"$0\":\"world\"}]}")
              -- TODO: when the batch fails
            , test "when the batch succeeds, writes root" <|
                \() ->
                    start
                        |> foldResults
                            [ resolveRead "root-v1" Nothing
                            , updateUi (TestApp.Typed "world")
                            , updateUi (TestApp.Add)
                            , TestContext.resolveMockTask
                                (.writeContent >> (|>) "{\"events\":[{\"tag\":\"AddItem\",\"$0\":\"world\"}]}")
                                (Ok "sha256-202d7ef7d01b4f103ca3e78536d82ed5bfdb57f31ee8588fe1f64e3fc70ab46e")
                            ]
                        |> expectMockTask
                            (.writeRef >> (\f -> f "root-v1" Nothing "sha256-202d7ef7d01b4f103ca3e78536d82ed5bfdb57f31ee8588fe1f64e3fc70ab46e"))
              -- TODO: write root correctly with a previous root
            ]
        ]
