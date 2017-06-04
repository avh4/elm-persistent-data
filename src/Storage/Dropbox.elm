module Storage.Dropbox exposing (storage)

{-| A `Storage` implementation that stores files in [Dropbox](https://dropbox.com).
-}

import Dropbox
import Storage exposing (Storage)
import Storage.Hash as Hash
import Task exposing (Task)


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
                -- TODO: if the Storage interface provided a way for stores to return revision ids when reading,
                -- then we could avoid doing the extra read here
                let
                    path =
                        "/refs/" ++ key

                    fetchOldRev : Task String String
                    fetchOldRev =
                        Dropbox.download auth { path = path }
                            |> Task.mapError toString
                            |> Task.andThen verifyContent

                    verifyContent : Dropbox.DownloadResponse -> Task String String
                    verifyContent response =
                        if Just response.content == oldValue then
                            Task.succeed response.rev
                        else
                            Task.fail (key ++ ": Existing content doesn't match")

                    putNew : String -> Task String ()
                    putNew oldRev =
                        Dropbox.upload auth
                            { path = "/refs/" ++ key
                            , mode = Dropbox.Update oldRev
                            , autorename = False
                            , clientModified = Nothing
                            , mute = True
                            , content = newValue
                            }
                            |> Task.map (always ())
                            |> Task.mapError toString
                in
                fetchOldRev
                    |> Task.andThen putNew
        }
    , content =
        { read =
            \hash ->
                Dropbox.download auth
                    { path = "/content/" ++ Hash.toString hash
                    }
                    |> Task.map .content
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
