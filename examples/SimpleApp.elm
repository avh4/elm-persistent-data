module SimpleApp exposing (main)

import BeautifulExample
import Color
import Json.Decode
import Json.Encode
import Persistence.Simple
import PersistentCache
import TestApp


main =
    Persistence.Simple.program
        { appId = TestApp.appId
        , data = TestApp.data
        , dataCache =
            { encoder = TestApp.dataEncoder
            , decoder = TestApp.dataDecoder
            }
        , ui =
            let
                ({ view } as ui) =
                    TestApp.ui
            in
            { ui
                | view =
                    \data state ->
                        view data state
                            |> BeautifulExample.view
                                { title = "Persistence.Simple demo"
                                , details = Just """This is an example of using Persistence.Simple which provides a standard authentication UI making it simple to give your users a choice of where to store their data."""
                                , maxWidth = 600
                                , githubUrl = Just "https://github.com/avh4/elm-persistent-data/blob/master/examples/SimpleApp.elm"
                                , documentationUrl = Nothing
                                , color = Just Color.charcoal
                                }
            }
        , localStorage =
            { get = PersistentCache.get cache
            , add = PersistentCache.add cache
            }
        , serviceAppKeys =
            { dropbox = Nothing
            }
        }


cache =
    PersistentCache.cache
        { name = "SimpleApp"
        , version = 1
        , kilobytes = 10
        , decode = Json.Decode.string
        , encode = Json.Encode.string
        }
