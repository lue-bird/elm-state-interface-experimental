module List.LocalExtra exposing (appendFast, atIndex, firstJustMap, foldUpIndexedFrom, justs, justsMapIndexed, mapAnyOrder)


appendFast : List a -> List a -> List a
appendFast listA listB =
    case listA of
        [] ->
            listB

        x :: xs ->
            appendFast xs (x :: listB)


atIndex : Int -> (List a -> Maybe a)
atIndex index sticks =
    if index <= -1 then
        Nothing

    else
        case sticks of
            [] ->
                Nothing

            head :: tail ->
                if index <= 0 then
                    head |> Just

                else
                    atIndex (index - 1) tail


firstJustMap : (a -> Maybe b) -> List a -> Maybe b
firstJustMap elementToMaybeFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case elementToMaybeFound head of
                Nothing ->
                    firstJustMap elementToMaybeFound tail

                Just found ->
                    Just found


mapAnyOrder : (a -> b) -> (List a -> List b)
mapAnyOrder elementChange list =
    list |> mapAnyOrderOnto [] elementChange


mapAnyOrderOnto : List b -> (a -> b) -> (List a -> List b)
mapAnyOrderOnto soFar elementChange list =
    case list of
        [] ->
            soFar

        head :: tail ->
            mapAnyOrderOnto (elementChange head :: soFar)
                elementChange
                tail


justs : List (Maybe value) -> List value
justs =
    \list -> justsOnto [] list


justsOnto : List a -> (List (Maybe a) -> List a)
justsOnto soFar list =
    case list of
        [] ->
            soFar

        head :: tail ->
            case head of
                Nothing ->
                    justsOnto soFar tail

                Just value ->
                    justsOnto (value :: soFar) tail


justsMapIndexed : (Int -> element -> Maybe value) -> (List element -> List value)
justsMapIndexed elementToMaybe list =
    list
        |> List.foldr
            (\element soFar ->
                { index = soFar.index - 1
                , justs =
                    case element |> elementToMaybe soFar.index of
                        Nothing ->
                            soFar.justs

                        Just just ->
                            just :: soFar.justs
                }
            )
            { index = (list |> List.length) - 1
            , justs = []
            }
        |> .justs


foldUpIndexedFrom :
    folded
    -> (Int -> element -> (folded -> folded))
    -> (List element -> folded)
foldUpIndexedFrom initialFolded reduce list =
    list
        |> List.foldl
            (\element soFar ->
                { index = soFar.index + 1
                , folded = soFar.folded |> reduce soFar.index element
                }
            )
            { index = 0, folded = initialFolded }
        |> .folded
