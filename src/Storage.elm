module Storage exposing (Storage)

{-| This is the interface that must be implemented to connect a storage
implementaiton to `Persistence`.

@docs Storage

-}

import Task exposing (Task)


{-| Storage implementations must implement the following functions, each of
which returns Task that can give a String error message:

  - **read**: Returns the string currently stored for a given key, or `Nothing`
    if there is no value stored for that key.
  - **writeContent**: Stores a content string to the key
    "sha256-<sha256 of the content>"
  - **writeRef**: Updates the value stored for `key`, but only if its current
    value matches `oldValue`.

    myStorage =
        { read = \key -> ...
        , writeContent = \content -> ...
        , writeRef = \key oldValue newValue -> ...
        }

-}
type alias Storage =
    { read : String -> Task String (Maybe String)
    , writeContent : String -> Task String String
    , writeRef :
        String -> Maybe String -> String -> Task String ()
        -- , maxBlobSize : Int
    }
