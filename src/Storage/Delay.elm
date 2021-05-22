module Storage.Delay exposing (storage, refStore, contentStore, cache)

{-| Add delays to a `Storage` implementation to assist in debugging.

@docs storage, refStore, contentStore, cache

-}

import Duration exposing (Duration)
import Process
import Storage exposing (CacheStore, ContentStore, RefStore, Storage)
import Task


{-| Add delays to a `Storage` implementation.
-}
storage : Duration -> Storage -> Storage
storage delay impl =
    { refs = refStore delay impl.refs
    , content = contentStore delay impl.content
    }


{-| Add delays to a `RefStore`
-}
refStore : Duration -> RefStore -> RefStore
refStore delay impl =
    { read =
        \key ->
            Process.sleep (Duration.inMilliseconds delay)
                |> Task.andThen (\() -> impl.read key)
    , write =
        \key oldValue newValue ->
            Process.sleep (Duration.inMilliseconds delay)
                |> Task.andThen (\() -> impl.write key oldValue newValue)
    }


{-| Add delays to a `ContentStore`
-}
contentStore : Duration -> ContentStore -> ContentStore
contentStore delay impl =
    { read =
        \hash ->
            Process.sleep (Duration.inMilliseconds delay)
                |> Task.andThen (\() -> impl.read hash)
    , write =
        \value ->
            Process.sleep (Duration.inMilliseconds delay)
                |> Task.andThen (\() -> impl.write value)
    }


{-| Add delays to a `CacheStore`
-}
cache : Duration -> CacheStore -> CacheStore
cache delay impl =
    { read =
        Process.sleep (Duration.inMilliseconds delay)
            |> Task.andThen (\() -> impl.read)
    , write =
        \value ->
            Process.sleep (Duration.inMilliseconds delay)
                |> Task.andThen (\() -> impl.write value)
    }
