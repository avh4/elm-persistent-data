module TestAppWithCache exposing (main)

import BeautifulExample
import ChooseStorage
import Color
import PersistentCache
import ProgramRecord
import ProgramWithAuth
import Task
import TestApp


main =
    ProgramRecord.toProgram <|
        (\r ->
            { r
                | view =
                    BeautifulExample.view
                        { title = "elm-persistent-data"
                        , details = Nothing
                        , maxWidth = 600
                        , githubUrl = Just "https://github.com/avh4/elm-persistent-data"
                        , documentationUrl = Nothing
                        , color = Just Color.charcoal
                        }
                        << r.view
            }
        )
        <|
            ProgramWithAuth.authProgram
                { read =
                    PersistentCache.get cache "auth"
                        |> Task.andThen (Maybe.map Task.succeed >> Maybe.withDefault (Task.fail ()))
                , write =
                    PersistentCache.add cache "auth"
                        >> Task.map (always ())
                }
                ChooseStorage.programRecord
                (ChooseStorage.create >> uncurry TestApp.programRecord)


cache =
    PersistentCache.cache
        { name = "TestAppWithCache"
        , version = 1
        , kilobytes = 2000
        , decode = ChooseStorage.decoder
        , encode = ChooseStorage.encoder
        }
