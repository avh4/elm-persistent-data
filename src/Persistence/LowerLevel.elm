module Persistence.LowerLevel exposing (LowerLevel)

import Task exposing (Task)


type alias LowerLevel =
    { read : String -> Task String (Maybe String)
    , write : String -> String -> Task String ()
    }
