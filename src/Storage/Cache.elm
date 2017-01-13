module Storage.Cache exposing (cache)

import Storage exposing (Storage)
import Task


cache : Storage.ContentStore -> Storage.ContentStore -> Storage.ContentStore
cache fast slow =
    { read = \hash -> fast.read hash
    , write = \value -> Task.fail "TODO"
    }
