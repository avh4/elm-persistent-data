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
                                Task.fail (Debug.toString other)

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

                    ignoreNotFound err =
                        case err of
                            Dropbox.PathDownloadError _ ->
                                Task.succeed Nothing

                            _ ->
                                Task.fail err

                    fetchOldRev : Task String (Maybe String)
                    fetchOldRev =
                        Dropbox.download auth { path = path }
                            |> Task.map Just
                            |> Task.onError ignoreNotFound
                            |> Task.mapError Debug.toString
                            |> Task.andThen verifyContent

                    verifyContent : Maybe Dropbox.DownloadResponse -> Task String (Maybe String)
                    verifyContent response =
                        if Maybe.map .content response == oldValue then
                            Task.succeed (Maybe.map .rev response)

                        else
                            Task.fail (key ++ ": Existing content doesn't match")

                    putNew : Maybe String -> Task String ()
                    putNew oldRev =
                        Dropbox.upload auth
                            { path = "/refs/" ++ key
                            , mode =
                                oldRev
                                    |> Maybe.map Dropbox.Update
                                    |> Maybe.withDefault Dropbox.Add
                            , autorename = False
                            , clientModified = Nothing
                            , mute = True
                            , content = newValue
                            }
                            |> Task.map (always ())
                            |> Task.mapError Debug.toString
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
                    |> Task.mapError Debug.toString
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
                    |> Task.mapError Debug.toString
                    |> Task.map (always hash)
        }
    }
