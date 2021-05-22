module Storage.ExampleServer exposing (storage)

{-| An example `Storage` implementation using HTTP.

@docs storage

-}

import Http
import Storage exposing (Storage)
import Storage.Hash as Hash


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
                    resolve response =
                        case response of
                            Http.BadStatus_ metadata _ ->
                                if metadata.statusCode == 404 then
                                    Ok Nothing

                                else
                                    Err (Debug.toString response)

                            Http.GoodStatus_ _ body ->
                                Ok (Just body)

                            _ ->
                                Err (Debug.toString response)
                in
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = root ++ "/refs/" ++ key
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver resolve
                    , timeout = Nothing
                    }
        , write =
            \key oldValue newValue ->
                let
                    resolve response =
                        case response of
                            Http.GoodStatus_ _ _ ->
                                Ok ()

                            _ ->
                                Err (Debug.toString response)
                in
                Http.task
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
                    , resolver = Http.stringResolver resolve
                    , timeout = Nothing
                    }
        }
    , content =
        { read =
            \hash ->
                let
                    resolve response =
                        case response of
                            Http.GoodStatus_ _ body ->
                                Ok body

                            _ ->
                                Err (Debug.toString response)
                in
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = root ++ "/content/" ++ Hash.toString hash
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver resolve
                    , timeout = Nothing
                    }
        , write =
            \content ->
                let
                    hash =
                        Hash.ofString content

                    resolve response =
                        case response of
                            Http.GoodStatus_ _ _ ->
                                Ok hash

                            _ ->
                                Err (Debug.toString response)
                in
                Http.task
                    { method = "PUT"
                    , headers = []
                    , url = root ++ "/content/" ++ Hash.toString hash
                    , body = Http.stringBody "application/octet-stream" content
                    , resolver = Http.stringResolver resolve
                    , timeout = Nothing
                    }
        }
    }
