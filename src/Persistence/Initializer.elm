module Persistence.Initializer exposing (Next(..), init)


type Next id data msg
    = Done (Maybe id) data
    | FetchBatch id (Result ( List msg, Maybe id ) data -> Next id data msg)


init :
    { cachedData : Maybe { root : id, data : data }
    , latestRoot : Maybe id
    , emptyData : data
    , fold : msg -> data -> data
    }
    -> Next id data msg
init config =
    handleParent config.fold config.latestRoot config.latestRoot config.emptyData []


handleParent fold rootId next data allMsgs =
    case next of
        Nothing ->
            Done rootId (List.foldl fold data (List.concat allMsgs))

        Just parentId ->
            FetchBatch parentId
                (\batch ->
                    case batch of
                        Ok data ->
                            Done rootId (List.foldl fold data (List.concat allMsgs))

                        Err ( msgs, grandparent ) ->
                            handleParent fold rootId grandparent data (msgs :: allMsgs)
                )
