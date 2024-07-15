module Rope.LocalExtra exposing (mapFast)

import Rope exposing (Rope)


mapFast : (a -> b) -> Rope a -> Rope b
mapFast elementChange rope =
    rope
        |> Rope.foldr
            (\element soFar -> elementChange element :: soFar)
            []
        |> Rope.fromList
