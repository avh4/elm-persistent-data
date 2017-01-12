module Storage.ExampleServer exposing (storage)

{-| An example `Storage` implementation using HTTP.

@docs storage

-}

import Http
import Task
import Sha256
import Storage exposing (Storage)
import Storage.Hash as Hash


{-| An example `Storage` implementation using HTTP.
-}
storage : Storage
storage =
    { refs =
        { read =
            \key ->
                let
                    onError error =
                        case error of
                            Http.BadStatus response ->
                                if response.status.code == 404 then
                                    Task.succeed Nothing
                                else
                                    Task.fail (toString error)

                            _ ->
                                Task.fail (toString error)

                    parseHash string =
                        case Maybe.map Hash.fromString string of
                            Just (Ok hash) ->
                                Task.succeed (Just hash)

                            Just (Err message) ->
                                Task.fail message

                            Nothing ->
                                Task.succeed Nothing
                in
                    Http.getString ("/" ++ key)
                        |> Http.toTask
                        |> Task.map Just
                        |> Task.onError onError
                        |> Task.andThen parseHash
        , write =
            \key oldValue newValue ->
                Http.request
                    { method = "PUT"
                    , headers =
                        [ case oldValue of
                            Nothing ->
                                Http.header "x-if-empty" "true"

                            Just val ->
                                Http.header "x-if-match" (Hash.toString val)
                        ]
                    , url = ("/" ++ key)
                    , body =
                        Http.stringBody
                            "application/octet-stream"
                            (Hash.toString newValue)
                    , expect = Http.expectStringResponse (\_ -> Ok ())
                    , timeout = Nothing
                    , withCredentials = False
                    }
                    |> Http.toTask
                    |> Task.map (always ())
                    |> Task.mapError toString
        }
    , content =
        { read =
            \hash ->
                let
                    onError error =
                        case error of
                            Http.BadStatus response ->
                                if response.status.code == 404 then
                                    Task.succeed Nothing
                                else
                                    Task.fail (toString error)

                            _ ->
                                Task.fail (toString error)
                in
                    Http.getString ("/" ++ Hash.toString hash)
                        |> Http.toTask
                        |> Task.map Just
                        |> Task.onError onError
        , write =
            \content ->
                let
                    key =
                        Sha256.sha256 content
                in
                    Http.request
                        { method = "PUT"
                        , headers = []
                        , url = ("/" ++ "sha256-" ++ key)
                        , body = Http.stringBody "application/octet-stream" content
                        , expect = Http.expectStringResponse (\_ -> Ok ())
                        , timeout = Nothing
                        , withCredentials = False
                        }
                        |> Http.toTask
                        |> Task.map (always (Hash.Sha256 key))
                        |> Task.mapError toString
        }
    }
