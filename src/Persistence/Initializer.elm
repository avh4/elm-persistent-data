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
    handleParent config config.latestRoot []


handleParent config next allMsgs =
    let
        done d =
            Done config.latestRoot
                -- TODO: optimize by not using List.concat
                (List.foldl config.fold d (List.concat allMsgs))
    in
    case next of
        Nothing ->
            done config.emptyData

        Just parentId ->
            let
                fetchNext =
                    FetchBatch parentId
                        (\batch ->
                            case batch of
                                Ok data ->
                                    done data

                                Err ( msgs, grandparent ) ->
                                    handleParent config grandparent (msgs :: allMsgs)
                        )
            in
            case config.cachedData of
                Nothing ->
                    fetchNext

                Just cached ->
                    if cached.root == parentId then
                        done cached.data
                    else
                        fetchNext
