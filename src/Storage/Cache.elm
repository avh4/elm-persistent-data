module Storage.Cache exposing (cache, contentStore)

import Process
import Storage exposing (Storage)
import Storage.Hash exposing (Hash)
import Task exposing (Task)


cache : Storage -> Storage -> Storage
cache fast slow =
    { refs = slow.refs
    , content = contentStore fast.content slow.content
    }


contentStore : Storage.ContentStore -> Storage.ContentStore -> Storage.ContentStore
contentStore fast slow =
    let
        fastRead : Hash -> Task () String
        fastRead hash =
            fast.read hash
                |> Task.mapError (always ())
                |> Task.andThen
                    (Maybe.map Task.succeed
                        >> Maybe.withDefault (Task.fail ())
                    )

        fastWrite : Maybe String -> Task Never ()
        fastWrite content =
            case content of
                Nothing ->
                    Task.succeed ()

                Just value ->
                    fast.write value
                        |> Process.spawn
                        |> Task.map (always ())
    in
    { read =
        \hash ->
            fastRead hash
                |> Task.map Just
                |> Task.onError (\() -> slow.read hash)
                |> passThrough fastWrite
    , write = \value -> slow.write value
    }


passThrough : (a -> Task ignored ignored2) -> Task x a -> Task x a
passThrough ignored source =
    source
        |> Task.andThen
            (\x ->
                ignored x
                    |> Process.spawn
                    |> Task.map (always x)
            )
