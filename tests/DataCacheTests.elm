module DataCacheTests exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)
import TestContextWithMocks as TestContext exposing (TestContext, MockTask)
import Storage.Hash as Hash exposing (Hash)
import Persistence
import TestApp


all : Test
all =
    describe "Persistence data cache"
        [ test "when an event is processed, writes to the cache" <|
            \() ->
                let
                    batch =
                        """{"events":[{"tag":"AddItem","$0":"buy carrots"}],"parent":null}"""

                    finalData =
                        """{"root":\"""" ++ Hash.toString (hash batch) ++ """","data":{"list":["buy carrots"]}}"""
                in
                    start
                        |> resolve mocks.readData Nothing
                        |> resolve (mocks.readRef appId) Nothing
                        |> updateUi (TestApp.Typed "buy carrots")
                        |> updateUi (TestApp.Add)
                        |> resolve (mocks.writeContent batch) (hash batch)
                        |> expectMockTask (mocks.writeData finalData)
        , test "initial read, with no cached data" <|
            \() ->
                start
                    |> resolve mocks.readData Nothing
                    |> expectMockTask (mocks.readRef appId)
        , test "initial read, with cache and no new events" <|
            \() ->
                start
                    |> resolve mocks.readData (Just """{"root":"sha256-d5509674058d858ed72c44396671a1c6ddab91730a7af44799ad1e893fa64005","data":{"list":["XYZZY"]}}""")
                    |> resolve (mocks.readRef appId) (Just <| hash "__ROOT__")
                    |> expectCurrent
                        (Persistence.Ready
                            { list = [ "XYZZY" ] }
                            { input = "" }
                        )

        -- TODO: initial read, with new remote data and a common ancestor
        -- TODO: initial read, with new remote data and no common ancestor
        -- TODO: should write cache when initial events are loaded remotely
        ]


type alias Mocks =
    { readRef : String -> MockTask String (Maybe Hash)
    , readContent : Hash -> MockTask String (Maybe String)
    , writeContent : String -> MockTask String Hash
    , writeRef : String -> Maybe Hash -> Hash -> MockTask String ()
    , readData : MockTask Never (Maybe String)
    , writeData : String -> MockTask Never ()
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
    , readData =
        TestContext.mockTask "readData"
    , writeData =
        \content ->
            TestContext.mockTask ("writeData:" ++ content)
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
        (Just
            { read = mocks.readData |> TestContext.toTask
            , write = mocks.writeData >> TestContext.toTask
            }
        )
        |> TestContext.start


updateUi :
    msg
    -> TestContext model (Persistence.Msg event msg)
    -> TestContext model (Persistence.Msg event msg)
updateUi uiMsg =
    TestContext.update (uiMsg |> Persistence.uimsg)


foldResults : List (a -> Result x a) -> a -> Result x a
foldResults steps init =
    List.foldl (\f a -> Result.andThen f a) (Ok init) steps


expectMockTask :
    MockTask x a
    -> TestContext model msg
    -> Expectation
expectMockTask =
    TestContext.expectMockTask


expectOk : (a -> Expectation) -> Result String a -> Expectation
expectOk expectation result =
    case result of
        Err x ->
            Expect.fail x

        Ok a ->
            expectation a


resolve :
    MockTask error a
    -> a
    -> TestContext model msg
    -> TestContext model msg
resolve mock value =
    TestContext.resolveMockTask mock (Ok value)


expectCurrent :
    Persistence.PersistenceState data ui
    -> TestContext (Persistence.Model data ui) msg
    -> Expectation
expectCurrent expected =
    TestContext.expectModel
        (Persistence.current
            >> Expect.equal expected
        )


hash : String -> Hash
hash =
    Hash.ofString


appId : String
appId =
    "io.github.avh4.elm-persistent-data.test-app.root-v1"