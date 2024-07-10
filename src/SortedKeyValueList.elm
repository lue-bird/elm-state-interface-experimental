module SortedKeyValueList exposing (SortedKeyValueList, fromList, get, map, toList)

{-| Alternative to FastDict.Dict optimized for fast merge and fast creation.
Would be a terrible fit if we needed fast insert and get.
-}

import List.LocalExtra


type alias SortedKeyValueList key value =
    { sortedKeyValueList : List { key : key, value : value } }


map :
    (key -> value -> newValue)
    -> (SortedKeyValueList key value -> SortedKeyValueList key newValue)
map elementChange sortedKeyValueList =
    { sortedKeyValueList =
        sortedKeyValueList.sortedKeyValueList
            |> List.map
                (\entry ->
                    { key = entry.key, value = elementChange entry.key entry.value }
                )
    }


{-| Sort a given list of { key, value } elements to create a [`SortedKeyValueList`](#SortedKeyValueList)
-}
fromList : List { value : value, key : comparable } -> SortedKeyValueList comparable value
fromList =
    \unsortedList ->
        { sortedKeyValueList = unsortedList |> List.sortBy .key }


toList : SortedKeyValueList key value -> List { key : key, value : value }
toList sortedKeyValueList =
    sortedKeyValueList.sortedKeyValueList


{-| The fact that this can only be implemented linearly might seem shocking.
In reality, merging and creating a FastDict.Dict that gets thrown away after the next .get is way heavier (that's the theory at least).
-}
get : key -> SortedKeyValueList key value -> Maybe value
get keyToFind sortedKeyValueList =
    sortedKeyValueList
        |> toList
        |> List.LocalExtra.firstJustMap
            (\entry ->
                if entry.key == keyToFind then
                    Just entry.value

                else
                    Nothing
            )
