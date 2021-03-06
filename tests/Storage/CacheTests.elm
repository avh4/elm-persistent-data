module Storage.CacheTests exposing (all)

import Expect exposing (Expectation)
import Html
import Storage
import Storage.Cache as Cache
import Storage.Hash as Hash exposing (Hash)
import Task exposing (Task)
import Test exposing (..)
import TestContextWithMocks as TestContext exposing (MockTask, TestContext, mockTask, resolveMockTask)


fastRead : Hash -> MockTask String String
fastRead hash =
    mockTask ("fast.read " ++ Hash.toString hash)


fastWrite : String -> MockTask String Hash
fastWrite value =
    mockTask ("fast.write " ++ value)


slowRead : Hash -> MockTask String String
slowRead hash =
    mockTask ("slow.read " ++ Hash.toString hash)


slowWrite : String -> MockTask String Hash
slowWrite value =
    mockTask ("slow.write " ++ value)


cache : Storage.ContentStore
cache =
    Cache.contentStore
        { read = fastRead >> TestContext.toTask
        , write = fastWrite >> TestContext.toTask
        }
        { read = slowRead >> TestContext.toTask
        , write = slowWrite >> TestContext.toTask
        }


start : Task x a -> TestContext (Maybe (Result x a)) (Result x a)
start task =
    Html.program
        { init = ( Nothing, Task.attempt identity task )
        , update = \msg model -> ( Just msg, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> Html.text "view"
        }
        |> TestContext.start


testTask :
    String
    -> Task x a
    -> List (TestContext (Maybe (Result x a)) (Result x a) -> TestContext (Maybe (Result x a)) (Result x a))
    -> Result x a
    -> Test
testTask name task steps expected =
    test name <|
        \() ->
            testResults (start task)
                steps
                (TestContext.expectModel <| Expect.equal (Just expected))


all : Test
all =
    describe "Storage.Cache"
        [ describe "read"
            [ testTask "when the fast store has the value"
                (cache.read (Hash.ofString "ABC"))
                [ resolveMockTask (fastRead (Hash.ofString "ABC")) (Ok "ABC") ]
                (Ok "ABC")
            , testTask "when only the slow store has the value"
                (cache.read (Hash.ofString "ABC"))
                [ resolveMockTask (fastRead (Hash.ofString "ABC")) (Err "Doesn't exist")
                , resolveMockTask (slowRead (Hash.ofString "ABC")) (Ok "ABC")
                ]
                (Ok "ABC")
            , test "when slow store succeeds, writes to fast store" <|
                \() ->
                    testResults (start <| cache.read (Hash.ofString "ABC"))
                        [ resolveMockTask (fastRead (Hash.ofString "ABC")) (Err "Doesn't exist")
                        , resolveMockTask (slowRead (Hash.ofString "ABC")) (Ok "ABC")
                        ]
                        (TestContext.expectMockTask (fastWrite "ABC"))
            , testTask "when slow store fails, fails"
                (cache.read (Hash.ofString "ABC"))
                [ resolveMockTask (fastRead (Hash.ofString "ABC")) (Err "Doesn't exist")
                , resolveMockTask (slowRead (Hash.ofString "ABC")) (Err "X")
                ]
                (Err "X")
            , testTask "when fast store fails, ignores the failure"
                (cache.read (Hash.ofString "ABC"))
                [ resolveMockTask (fastRead (Hash.ofString "ABC")) (Err "X")
                , resolveMockTask (slowRead (Hash.ofString "ABC")) (Ok "ABC")
                ]
                (Ok "ABC")
            , testTask "when write to fast store fails, ignores the failure"
                (cache.read (Hash.ofString "ABC"))
                [ resolveMockTask (fastRead (Hash.ofString "ABC")) (Err "Doesn't exist")
                , resolveMockTask (slowRead (Hash.ofString "ABC")) (Ok "ABC")
                , resolveMockTask (fastWrite "ABC") (Err "X")
                ]
                (Ok "ABC")
            ]
        , describe "write"
            [ testTask "when slow store succeeds, succeeds"
                (cache.write "ABC")
                [ resolveMockTask (slowWrite "ABC") (Ok <| Hash.ofString "ABC") ]
                (Ok <| Hash.ofString "ABC")
            , testTask "when slow store fails, fails"
                (cache.write "ABC")
                [ resolveMockTask (slowWrite "ABC") (Err "X") ]
                (Err "X")
            ]
        ]


expectOk : (a -> Expectation) -> Result String a -> Expectation
expectOk expectation result =
    case result of
        Err x ->
            Expect.fail x

        Ok a ->
            expectation a


testResults : a -> List (a -> a) -> (a -> Expectation) -> Expectation
testResults init steps expect =
    List.foldl (\f a -> f a) init steps
        |> expect
