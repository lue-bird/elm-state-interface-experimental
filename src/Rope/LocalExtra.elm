module Rope.LocalExtra exposing (mapFast, mapFastAnyOrder)

import Rope exposing (Rope)


mapFast : (a -> b) -> Rope a -> Rope b
mapFast elementChange rope =
    rope
        |> Rope.foldr
            (\element soFar -> elementChange element :: soFar)
            []
        |> Rope.fromList


mapFastAnyOrder : (a -> b) -> Rope a -> Rope b
mapFastAnyOrder elementChange rope =
    rope
        |> Rope.foldl
            (\element soFar -> elementChange element :: soFar)
            []
        |> Rope.fromList
