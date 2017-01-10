module Storage.ExampleServer exposing (storage)

{-| An example `Storage` implementation using HTTP.

@docs storage

-}

import Http
import Task
import Sha256
import Storage exposing (Storage)


{-| An example `Storage` implementation using HTTP.
-}
storage : Storage
storage =
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
            in
                Http.getString ("/" ++ key)
                    |> Http.toTask
                    |> Task.map Just
                    |> Task.onError onError
    , writeContent =
        \blob ->
            let
                key =
                    "sha256-" ++ Sha256.sha256 blob
            in
                Http.request
                    { method = "PUT"
                    , headers = []
                    , url = ("/" ++ key)
                    , body = Http.stringBody "application/octet-stream" blob
                    , expect = Http.expectStringResponse (\_ -> Ok ())
                    , timeout = Nothing
                    , withCredentials = False
                    }
                    |> Http.toTask
                    |> Task.map (always key)
                    |> Task.mapError toString
    , writeRef =
        \key oldValue newValue ->
            Http.request
                { method = "PUT"
                , headers =
                    [ case oldValue of
                        Nothing ->
                            Http.header "x-if-empty" "true"

                        Just val ->
                            Http.header "x-if-match" val
                    ]
                , url = ("/" ++ key)
                , body = Http.stringBody "application/octet-stream" newValue
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
                |> Http.toTask
                |> Task.map (always ())
                |> Task.mapError toString
    }
