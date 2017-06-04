module Persistence.Initializer exposing (Next(..), init)


type Next id data msg
    = Done (Maybe id) data
    | FetchBatch id (Result (List msg) data -> Next id data msg)


init :
    { cachedData : Maybe { root : id, data : data }
    , latestRoot : Maybe id
    , emptyData : data
    , fold : msg -> data -> data
    }
    -> Next id data msg
init config =
    case config.latestRoot of
        Nothing ->
            Done Nothing config.emptyData

        Just rootId ->
            FetchBatch rootId
                (\batch ->
                    case batch of
                        Ok data ->
                            Done (Just rootId) data

                        Err msgs ->
                            Done (Just rootId) (List.foldr config.fold config.emptyData msgs)
                )
