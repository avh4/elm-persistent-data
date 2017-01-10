module Storage exposing (Storage)

import Task exposing (Task)


type alias Storage =
    { read : String -> Task String (Maybe String)
    , writeContent : String -> Task String String
    , writeRef :
        String -> Maybe String -> String -> Task String ()
        -- , maxBlobSize : Int
    }
