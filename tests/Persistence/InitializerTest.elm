module Persistence.InitializerTest exposing (all)

import Expect exposing (Expectation)
import Persistence.Initializer as Initializer
import Test exposing (..)


init config =
    Initializer.init
        { cachedData = config.cachedData
        , latestRoot = config.latestRoot
        , emptyData = ""
        , fold = (++)
        }
        |> Ok


resolveBatch : id -> Result (List msg) data -> Result String (Initializer.Next id data msg) -> Result String (Initializer.Next id data msg)
resolveBatch id result =
    let
        resolve prev =
            case prev of
                Initializer.FetchBatch actualId next ->
                    if actualId /= id then
                        Err ("Tried to resolve batch id=" ++ toString id ++ ", but " ++ toString prev ++ " was requested")
                    else
                        Ok <| next result

                _ ->
                    Err ("Tried to resolve batch id=" ++ toString id ++ ", but a batch was not requested: " ++ toString prev)
    in
    Result.andThen resolve


done : Maybe id -> data -> Result String (Initializer.Next id data msg) -> Expectation
done id data =
    Expect.equal (Ok <| Initializer.Done id data)


all : Test
all =
    describe "Persistnece.Initializer"
        [ test "with no initial data, uses emptyData" <|
            \() ->
                init
                    { cachedData = Nothing
                    , latestRoot = Nothing
                    }
                    |> done Nothing ""
        , describe "with initial root, fetches the batch"
            [ test "when first batch is cached" <|
                \() ->
                    init
                        { cachedData = Nothing
                        , latestRoot = Just 1
                        }
                        |> resolveBatch 1 (Ok "(A)")
                        |> done (Just 1) "(A)"
            , test "when first batch is fetched" <|
                \() ->
                    init
                        { cachedData = Nothing
                        , latestRoot = Just 1
                        }
                        |> resolveBatch 1 (Err [ "A", "B" ])
                        |> done (Just 1) "AB"
            ]
        ]
