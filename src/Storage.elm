module Storage exposing (CacheStore, ContentStore, RefStore, Storage)

{-| This is the interface that must be implemented to connect a storage
implementaiton to `Persistence`.

@docs Storage, RefStore, ContentStore, CacheStore

-}

import Storage.Hash exposing (Hash)
import Task exposing (Task)


{-| A Storage implementation consists of a `RefStore` and a `ContentStore`.

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
    { refs : RefStore
    , content : ContentStore
    }


{-| A key-value store where the keys are Strings.
To write a value for a key, you must correctly provide the current value.
-}
type alias RefStore =
    { read : String -> Task String (Maybe String)
    , write : String -> Maybe String -> String -> Task String ()
    }


{-| An immutable, content-addressable value store where the values are Strings.
The key of any value is the SHA-256 hash of the value.
-}
type alias ContentStore =
    { read : Hash -> Task String String
    , write : String -> Task String Hash
    }


{-| A mutable store that can store a single value. There is no gaurantee
that a stored value will be retrievable later.
-}
type alias CacheStore =
    { read : Task Never (Maybe String)
    , write : String -> Task Never ()
    }
