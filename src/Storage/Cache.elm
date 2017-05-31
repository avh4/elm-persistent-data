module Storage.Cache exposing (cache, contentStore)

import Process
import Storage exposing (Storage)
import Task


cache : Storage -> Storage -> Storage
cache fast slow =
    { refs = slow.refs
    , content = contentStore fast.content slow.content
    }


contentStore : Storage.ContentStore -> Storage.ContentStore -> Storage.ContentStore
contentStore fast slow =
    { read =
        \hash ->
            fast.read hash
                |> Task.andThen
                    (\fastValue ->
                        case fastValue of
                            Just x ->
                                Task.succeed fastValue

                            Nothing ->
                                slow.read hash
                                    |> Task.andThen
                                        (\slowValue ->
                                            case slowValue of
                                                Nothing ->
                                                    Task.succeed Nothing

                                                Just v ->
                                                    fast.write v
                                                        |> Process.spawn
                                                        |> Task.map (always slowValue)
                                        )
                    )
    , write = \value -> slow.write value
    }
