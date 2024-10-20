module TypeDistance exposing (distance)

{-| Mostly copied from <https://github.com/klaftertief/elm-search> (BSD-3-Clause licensed but no LICENSE file)
-}

import Dict exposing (Dict)
import Elm.Type
import List.Extra


distance : Elm.Type.Type -> Elm.Type.Type -> Float
distance needle hay =
    case ( needle, hay ) of
        {- Compare two functions `Function (List Type) Type`
           Functions get parsed like `a -> b` ~> `Function ([Var "a"]) (Var "b")`
           TODO: support three different comparisons
             - strict: length of arguments have to match
             - from beginning: concat args and result and compare the list
             - from end: concat args and result and compare the reversed list
           TODO: add some kind of mapping for vars in fuzzy calculations
        -}
        ( Elm.Type.Lambda argsN resultN, Elm.Type.Lambda argsH resultH ) ->
            let
                -- Handle special cases with singleton `Var` Type args
                argsDistance =
                    case ( argsN, argsH ) of
                        -- Compare `a -> r` and `b -> s`
                        ( Elm.Type.Var n, Elm.Type.Var h ) ->
                            varNameDistance n h

                        -- Compare `a -> r` and `b -> c -> s`
                        -- This is the important special case.
                        ( Elm.Type.Var _, _ ) ->
                            mediumPenalty

                        -- The default case
                        _ ->
                            distance argsN argsH

                resultDistance =
                    distance resultN resultH
            in
            (argsDistance + resultDistance) / 2

        -- `Var String`
        -- `a` ~> `Var "a"`
        ( Elm.Type.Var nameN, Elm.Type.Var nameH ) ->
            varNameDistance nameN nameH

        -- Special cases for comparisons like `number` - `Float`
        ( Elm.Type.Var nameN, Elm.Type.Type canonicalH [] ) ->
            distanceVarApply nameN canonicalH

        ( Elm.Type.Type canonicalN [], Elm.Type.Var nameH ) ->
            distanceVarApply nameH canonicalN

        -- Hack for special cases like `a` - `Maybe a`
        -- TODO: make proper comparison
        ( Elm.Type.Var nameN, Elm.Type.Type canonicalH argsH ) ->
            distanceApply ( "", [ Elm.Type.Var nameN ] ) ( canonicalH, argsH )

        ( Elm.Type.Type canonicalN argsN, Elm.Type.Var nameH ) ->
            distanceApply ( "", [ Elm.Type.Var nameH ] ) ( canonicalN, argsN )

        -- `Apply Name (List Type)`
        -- `Foo.Bar a b` ~> `Apply { home = "Foo", name = "Bar" } ([Var "a", Var "b"])`
        ( Elm.Type.Type canonicalN argsN, Elm.Type.Type canonicalH argsH ) ->
            distanceApply ( canonicalN, argsN ) ( canonicalH, argsH )

        -- Tuple (List Type)
        -- `(a,b)` ~> `Tuple ([Var "a",Var "b"])`
        ( Elm.Type.Tuple argsN, Elm.Type.Tuple argsH ) ->
            distanceList argsN argsH

        -- TODO: Record (List ( String, Type )) (Maybe String)
        {- The incomparable case
           TODO: Find and add special cases
        -}
        _ ->
            maxPenalty


distanceList : List Elm.Type.Type -> List Elm.Type.Type -> Float
distanceList needle hay =
    let
        needleLength : Int
        needleLength =
            List.length needle

        hayLength : Int
        hayLength =
            List.length hay

        maxLength : Int
        maxLength =
            max needleLength hayLength

        diffLength : Int
        diffLength =
            maxLength - min needleLength hayLength
    in
    if diffLength > 1 then
        maxPenalty

    else
        -- TODO: optimize, maybe add penalty for permutations
        List.Extra.permutations needle
            |> List.map
                (\permutation ->
                    ((List.map2 distance permutation hay |> List.sum)
                        + (toFloat diffLength * maxPenalty)
                    )
                        / toFloat maxLength
                )
            |> List.minimum
            |> Maybe.withDefault maxPenalty


applyNameDistance : String -> String -> Float
applyNameDistance needle hay =
    if needle == hay then
        noPenalty

    else if String.contains needle hay || String.contains hay needle then
        lowPenalty

    else
        highPenalty


varNameDistance : String -> String -> Float
varNameDistance needle hay =
    if needle == hay then
        noPenalty

    else if String.contains needle hay || String.contains hay needle then
        lowPenalty

    else
        mediumPenalty


reservedVars : Dict String (List String)
reservedVars =
    Dict.empty
        |> Dict.insert "number" [ "Float", "Int" ]
        |> Dict.insert "comparable" [ "Float", "Int", "Char", "String", "Posix" ]
        |> Dict.insert "appendable" [ "String", "List" ]


distanceVarApply : String -> String -> Float
distanceVarApply varName applyName =
    let
        maybeReservedVarTypeList : Maybe (List String)
        maybeReservedVarTypeList =
            Dict.get varName reservedVars
    in
    case maybeReservedVarTypeList of
        Just typeList ->
            if typeList |> List.any (\typeName -> typeName == applyName) then
                lowPenalty

            else
                maxPenalty

        Nothing ->
            mediumPenalty


distanceApply : ( String, List Elm.Type.Type ) -> ( String, List Elm.Type.Type ) -> Float
distanceApply ( nName, nArguments ) ( hName, hArguments ) =
    case ( nArguments, hArguments ) of
        ( [], [] ) ->
            applyNameDistance nName hName

        ( [], hd :: tl ) ->
            --distanceCanonical canonicalN canonicalH
            -- TODO: should we do this only for some specific types like `Maybe` and `Result`?
            -- TODO: check if this is a nice implementation (with regard to `min` and `+ lowPenalty`)
            min maxPenalty
                (distance (Elm.Type.Type nName nArguments)
                    (Maybe.withDefault hd (List.head (List.reverse tl)))
                    + lowPenalty
                )

        _ ->
            (applyNameDistance nName hName + distanceList nArguments hArguments) / 2


noPenalty : Float
noPenalty =
    0


lowPenalty : Float
lowPenalty =
    0.25


mediumPenalty : Float
mediumPenalty =
    0.5


highPenalty : Float
highPenalty =
    0.75


maxPenalty : Float
maxPenalty =
    1
