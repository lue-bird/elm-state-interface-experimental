module Tests exposing (tests)

import AsciiString
import Bytes
import Bytes.Encode
import Expect
import Fuzz
import List.LocalExtra
import StructuredId exposing (StructuredId)
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-state-interface-experimental"
        [ Test.describe "StructuredId"
            [ Test.fuzz
                (Fuzz.constant (\a b -> { a = a, b = b })
                    |> Fuzz.andMap treeFuzz
                    |> Fuzz.andMap treeFuzz
                )
                "== equivalent to on toString =="
                (\trees ->
                    (trees.a |> exampleTreeToStructuredId |> StructuredId.toString)
                        == (trees.b |> exampleTreeToStructuredId |> StructuredId.toString)
                        |> Expect.equal
                            (trees.a == trees.b)
                )
            ]
        , Test.describe "List.LocalExtra"
            [ Test.fuzz (Fuzz.list (Fuzz.maybe Fuzz.int))
                "justsMapIndexed"
                (\list ->
                    list
                        |> List.LocalExtra.justsMapIndexed
                            (\index maybe ->
                                maybe |> Maybe.map (\just -> { just = just, index = index })
                            )
                        |> Expect.equalLists
                            (list
                                |> List.indexedMap
                                    (\index maybe ->
                                        maybe |> Maybe.map (\just -> { just = just, index = index })
                                    )
                                |> List.filterMap identity
                            )
                )
            ]
        , Test.describe "bytes intermediate representation: ascii string"
            [ Test.fuzz asciiStringFuzz
                "roundtrip over bytes"
                (\asciiString ->
                    asciiString
                        |> AsciiString.toBytes
                        |> AsciiString.fromBytes
                        |> Expect.equal asciiString
                )
            , Test.fuzz bytesFuzz
                "roundtrip over ascii string"
                (\bytes ->
                    bytes
                        |> AsciiString.fromBytes
                        |> AsciiString.toBytes
                        |> Expect.equal bytes
                )
            ]
        ]


treeFuzz : Fuzz.Fuzzer ExampleTree
treeFuzz =
    treeFuzzAtDepth 0


treeFuzzAtDepth : Int -> Fuzz.Fuzzer ExampleTree
treeFuzzAtDepth depth =
    if depth >= 4 then
        Fuzz.map Leaf Fuzz.int

    else
        Fuzz.oneOf
            [ Fuzz.map Leaf Fuzz.int
            , Fuzz.map Branch (Fuzz.listOfLengthBetween 0 8 (treeFuzzAtDepth (depth + 1)))
            ]


exampleTreeToStructuredId : ExampleTree -> StructuredId
exampleTreeToStructuredId tree =
    case tree of
        Leaf int ->
            int |> StructuredId.ofInt

        Branch forest ->
            forest |> StructuredId.ofList exampleTreeToStructuredId


type ExampleTree
    = Leaf Int
    | Branch (List ExampleTree)


bytesFuzz : Fuzz.Fuzzer Bytes.Bytes
bytesFuzz =
    Fuzz.listOfLengthBetween 0
        30
        (Fuzz.intRange 0 255)
        |> Fuzz.map
            (\unsignedInt8s ->
                unsignedInt8s
                    |> List.map Bytes.Encode.unsignedInt8
                    |> Bytes.Encode.sequence
                    |> Bytes.Encode.encode
            )


asciiStringFuzz : Fuzz.Fuzzer String
asciiStringFuzz =
    Fuzz.listOfLengthBetween 0
        30
        Fuzz.asciiChar
        |> Fuzz.map String.fromList
