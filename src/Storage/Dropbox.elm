module Storage.Dropbox exposing (storage)

{-| A `Storage` implementation that stores files in [Dropbox](https://dropbox.com).
-}

import Dropbox
import Storage exposing (Storage)
import Storage.Hash as Hash
import Task


storage : Dropbox.UserAuth -> Storage
storage auth =
    { refs =
        { read =
            \key ->
                let
                    parse result =
                        case result of
                            Err (Dropbox.PathDownloadError _) ->
                                Task.succeed Nothing

                            Err other ->
                                Task.fail (toString other)

                            Ok response ->
                                Task.succeed <| Just response.content

                    taskToResult task =
                        task
                            |> Task.map Ok
                            |> Task.onError (Err >> Task.succeed)
                in
                Dropbox.download auth
                    { path = "/refs/" ++ key
                    }
                    |> taskToResult
                    |> Task.andThen parse
        , write =
            \key oldValue newValue ->
                -- TODO: verify old content
                Dropbox.upload auth
                    { path = "/refs/" ++ key
                    , mode = Dropbox.Overwrite -- TODO: use Update
                    , autorename = False
                    , clientModified = Nothing
                    , mute = True
                    , content = newValue
                    }
                    |> Task.map (always ())
                    |> Task.mapError toString
        }
    , content =
        { read =
            \hash ->
                Dropbox.download auth
                    { path = "/content/" ++ Hash.toString hash
                    }
                    |> Task.map (.content >> Just)
                    |> Task.mapError toString
        , write =
            \content ->
                let
                    hash =
                        Hash.ofString content
                in
                Dropbox.upload auth
                    { path = "/content/" ++ Hash.toString hash
                    , mode = Dropbox.Add
                    , autorename = False
                    , clientModified = Nothing
                    , mute = True
                    , content = content
                    }
                    |> Task.mapError toString
                    |> Task.map (always hash)
        }
    }
