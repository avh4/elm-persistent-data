module Storage.Cache exposing (cache)

import Storage exposing (Storage)
import Task


cache : Storage.ContentStore -> Storage.ContentStore -> Storage.ContentStore
cache fast slow =
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
                    )
    , write = \value -> Task.fail "TODO"
    }
