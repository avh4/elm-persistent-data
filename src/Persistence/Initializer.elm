module Persistence.Initializer exposing (Config, Next(..), init)


type alias Config id data msg =
    { cachedData : Maybe { root : id, data : data }
    , latestRoot : Maybe id
    , emptyData : data
    , fold : msg -> data -> data
    }


{-| The result of initializing an event store.

  - Done: initialization is complete, and the resulting root id and data are provided
  - FetchBatch: to continue initialization, you must fetch that batch of the given id and call the given function.
    If you have computed data cached for that id, you may pass `Ok data`,
    otherwise fetch the batch and pass `Err (batchMsgs, batchParentId)`.

-}
type Next id data msg
    = Done (Maybe id) data
    | FetchBatch id (Result ( List msg, Maybe id ) data -> Next id data msg)


init : Config id data msg -> Next id data msg
init config =
    handleParent config config.latestRoot []


handleParent : Config id data msg -> Maybe id -> List (List msg) -> Next id data msg
handleParent config next allMsgs =
    let
        done d =
            Done config.latestRoot
                -- TODO: optimize by using a nested fold instead of List.concat
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
