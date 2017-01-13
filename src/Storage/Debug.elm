module Storage.Debug exposing (storage, refStore, contentStore)

{-| Add debugging out to a `Storage` implementation.

@docs storage, refStore, contentStore
-}

import Storage exposing (Storage, RefStore, ContentStore)
import Task
import Storage.Hash as Hash


{-| Add debugging out to a `Storage` implementation.
-}
storage : String -> Storage -> Storage
storage label impl =
    { refs = refStore label impl.refs
    , content = contentStore label impl.content
    }


{-| Add debugging out to a `RefStore` implementation.
-}
refStore : String -> RefStore a -> RefStore a
refStore label impl =
    { read =
        \key ->
            impl.read (Debug.log (label ++ ": refs.read") key)
                |> Task.map (Debug.log (label ++ ": refs.read: " ++ key ++ ": Ok"))
                |> Task.mapError (Debug.log (label ++ ": refs.read: " ++ key ++ ": Err"))
    , write =
        \key oldValue newValue ->
            impl.write key oldValue (Debug.log (label ++ ": refs.write: " ++ key) newValue)
                |> Task.map (Debug.log (label ++ ": refs.write: " ++ key ++ ": Ok"))
                |> Task.mapError (Debug.log (label ++ ": refs.write: " ++ key ++ ": Err"))
    }


{-| Add debugging out to a `ContentStore` implementation.
-}
contentStore : String -> ContentStore -> ContentStore
contentStore label impl =
    { read =
        \hash ->
            impl.read hash
                |> Task.map (Debug.log (label ++ ": content.read: Ok: " ++ Hash.toString hash))
                |> Task.mapError (Debug.log (label ++ ": content.read: Err: " ++ Hash.toString hash))
    , write =
        \value ->
            impl.write (Debug.log (label ++ ": content.write") value)
                |> Task.map (Debug.log (label ++ ": content.write: Ok"))
                |> Task.mapError (Debug.log (label ++ ": content.write: Err"))
    }
