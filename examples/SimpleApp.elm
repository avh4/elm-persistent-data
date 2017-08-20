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
                ui =
                    TestApp.ui
            in
            { ui
                | init = \() -> ui.init
                , view =
                    \data state ->
                        ui.view data state
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
            { dropbox =
                -- This app is configured for https://avh4.github.io/elm-persistent-data/SimpleApp.elm.html
                -- If you are running this example at a different URL, you will need to create your own Dropbox App,
                -- enter the "App key" here, add this page's URL to the app's redirect URLs, and allow implicit grant.
                Just "gpoqst938n3ic81"
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
