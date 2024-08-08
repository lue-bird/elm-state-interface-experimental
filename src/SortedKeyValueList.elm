module SortedKeyValueList exposing (SortedKeyValueList, fromList, fromListBy, get, map, mergeBy, toList)

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


{-| Sort a given list of { key, value } elements
by a given comparable-ized representation of the key
to create a [`SortedKeyValueList`](#SortedKeyValueList)
-}
fromListBy :
    (key -> comparable_)
    -> (List { value : value, key : key } -> SortedKeyValueList key value)
fromListBy keyToComparable unsortedList =
    { sortedKeyValueList =
        unsortedList
            |> List.sortBy (\entry -> entry.key |> keyToComparable)
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


{-| Fold the lists of 2 [`SortedKeyValueList`](#SortedKeyValueList)s depending on where keys are present.
The idea and API is the same as [`Dict.merge`](https://dark.elm.dmy.fr/packages/elm/core/latest/Dict#merge)
-}
mergeBy :
    (key -> comparable_)
    -> (key -> a -> folded -> folded)
    -> (key -> a -> b -> folded -> folded)
    -> (key -> b -> folded -> folded)
    -> List { key : key, value : a }
    -> List { key : key, value : b }
    -> folded
    -> folded
mergeBy keyToComparable onlyA bothAB onlyB aSortedKeyValueList bSortedKeyValueList initialFolded =
    case aSortedKeyValueList of
        [] ->
            bSortedKeyValueList |> List.foldl (\entry soFar -> onlyB entry.key entry.value soFar) initialFolded

        aLowest :: aWithoutLowest ->
            case bSortedKeyValueList of
                [] ->
                    aWithoutLowest
                        |> List.foldl (\entry soFar -> onlyA entry.key entry.value soFar)
                            (onlyA aLowest.key aLowest.value initialFolded)

                bLowest :: bWithoutLowest ->
                    case compare (aLowest.key |> keyToComparable) (bLowest.key |> keyToComparable) of
                        EQ ->
                            mergeBy keyToComparable
                                onlyA
                                bothAB
                                onlyB
                                aWithoutLowest
                                bWithoutLowest
                                (bothAB aLowest.key aLowest.value bLowest.value initialFolded)

                        LT ->
                            mergeBy keyToComparable
                                onlyA
                                bothAB
                                onlyB
                                aWithoutLowest
                                bSortedKeyValueList
                                (onlyA aLowest.key aLowest.value initialFolded)

                        GT ->
                            mergeBy keyToComparable
                                onlyA
                                bothAB
                                onlyB
                                aSortedKeyValueList
                                bWithoutLowest
                                (onlyB bLowest.key bLowest.value initialFolded)
