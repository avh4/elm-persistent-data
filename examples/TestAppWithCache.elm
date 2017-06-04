module TestAppWithCache exposing (main)

import BeautifulExample
import ChooseStorage
import Color
import Json.Decode
import Json.Encode
import PersistentCache
import ProgramWithAuth
import TestApp


main =
    ProgramWithAuth.toProgram <|
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
                ChooseStorage.programRecord
                (\( storage, dataCache ) -> TestApp.programRecord storage dataCache)
