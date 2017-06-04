module Storage.Delay exposing (cache, contentStore, refStore, storage)

{-| Add delays to a `Storage` implementation to assist in debugging.

@docs storage, refStore, contentStore, cache

-}

import Process
import Storage exposing (CacheStore, ContentStore, RefStore, Storage)
import Task
import Time exposing (Time)


{-| Add delays to a `Storage` implementation.
-}
storage : Time -> Storage -> Storage
storage delay impl =
    { refs = refStore delay impl.refs
    , content = contentStore delay impl.content
    }


{-| Add delays to a `RefStore`
-}
refStore : Time -> RefStore -> RefStore
refStore delay impl =
    { read =
        \key ->
            Process.sleep delay
                |> Task.andThen (\() -> impl.read key)
    , write =
        \key oldValue newValue ->
            Process.sleep delay
                |> Task.andThen (\() -> impl.write key oldValue newValue)
    }


{-| Add delays to a `ContentStore`
-}
contentStore : Time -> ContentStore -> ContentStore
contentStore delay impl =
    { read =
        \hash ->
            Process.sleep delay
                |> Task.andThen (\() -> impl.read hash)
    , write =
        \value ->
            Process.sleep delay
                |> Task.andThen (\() -> impl.write value)
    }


{-| Add delays to a `CacheStore`
-}
cache : Time -> CacheStore -> CacheStore
cache delay impl =
    { read =
        Process.sleep delay
            |> Task.andThen (\() -> impl.read)
    , write =
        \value ->
            Process.sleep delay
                |> Task.andThen (\() -> impl.write value)
    }
