module SortedKeyValueList exposing (fromList, fromListBy, map)

{-| Alternative to FastDict.Dict optimized for fast merge and fast creation.
Would be a terrible fit if we needed fast insert and get.
-}

import Web exposing (SortedKeyValueList(..))


map :
    ({ key : key, value : value } -> newValue)
    -> (SortedKeyValueList key value -> SortedKeyValueList key newValue)
map elementChange (SortedKeyValueList sortedKeyValueList) =
    SortedKeyValueList
        (sortedKeyValueList
            |> List.map
                (\entry ->
                    { key = entry.key, value = elementChange entry }
                )
        )


{-| Sort a given list of { key, value } elements
by a given comparable-ized representation of the key
to create a [`SortedKeyValueList`](Web#SortedKeyValueList)
-}
fromListBy :
    (key -> comparable_)
    -> (List { value : value, key : key } -> SortedKeyValueList key value)
fromListBy keyToComparable unsortedList =
    SortedKeyValueList
        (unsortedList
            |> List.sortBy (\entry -> entry.key |> keyToComparable)
        )


{-| Sort a given list of { key, value } elements to create a [`SortedKeyValueList`](Web#SortedKeyValueList)
-}
fromList : List { value : value, key : comparable } -> SortedKeyValueList comparable value
fromList =
    \unsortedList ->
        SortedKeyValueList (unsortedList |> List.sortBy .key)
