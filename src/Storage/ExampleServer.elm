module Storage.ExampleServer exposing (storage)

{-| An example `Storage` implementation using HTTP.

@docs storage

-}

import Http
import Storage exposing (Storage)
import Storage.Hash as Hash
import Task


{-| An example `Storage` implementation using HTTP.
-}
storage : String -> Storage
storage rootUrl =
    let
        root =
            if String.endsWith "/" rootUrl then
                String.dropRight 1 rootUrl
            else
                rootUrl
    in
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
                in
                Http.getString (root ++ "/refs/" ++ key)
                    |> Http.toTask
                    |> Task.map Just
                    |> Task.onError onError
        , write =
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
                    , url = root ++ "/refs/" ++ key
                    , body =
                        Http.stringBody
                            "application/octet-stream"
                            newValue
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
                Http.getString (root ++ "/content/" ++ Hash.toString hash)
                    |> Http.toTask
                    |> Task.mapError toString
        , write =
            \content ->
                let
                    hash =
                        Hash.ofString content
                in
                Http.request
                    { method = "PUT"
                    , headers = []
                    , url = root ++ "/content/" ++ Hash.toString hash
                    , body = Http.stringBody "application/octet-stream" content
                    , expect = Http.expectStringResponse (\_ -> Ok ())
                    , timeout = Nothing
                    , withCredentials = False
                    }
                    |> Http.toTask
                    |> Task.map (always hash)
                    |> Task.mapError toString
        }
    }
