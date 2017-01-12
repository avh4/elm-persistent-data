module Storage exposing (Storage)

{-| This is the interface that must be implemented to connect a storage
implementaiton to `Persistence`.

@docs Storage

-}

import Task exposing (Task)
import Storage.Hash exposing (Hash)


{-| Storage implementations must implement the following functions, each of
which returns Task that can give a String error message:

`refs` is a key-value store where the keys are Strings and the values are content Hashes.
To write a value for a key, you must correctly provide the current value.

`content` is an immutable, content-addressable value store where the values are Strings.
The key of any value is the SHA-256 hash of the value.

    myStorage =
        { refs =
            { read = \key -> ...
            , write = \key oldValue newValue -> ...
            }
        , content =
            { read = \hash -> ...
            , write = \content -> ...
            }
        }

-}
type alias Storage =
    { refs :
        { read : String -> Task String (Maybe Hash)
        , write : String -> Maybe Hash -> Hash -> Task String ()
        }
    , content :
        { read : Hash -> Task String (Maybe String)
        , write : String -> Task String Hash
        }
        -- , maxBlobSize : Int
    }
