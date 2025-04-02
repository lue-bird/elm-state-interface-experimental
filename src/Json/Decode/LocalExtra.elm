module Json.Decode.LocalExtra exposing (choice, onlyString, resultOkErr)

import Json.Decode
import List.LocalExtra


choice : List { tag : String, value : Json.Decode.Decoder value } -> Json.Decode.Decoder value
choice variantDecoders =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                case
                    variantDecoders
                        |> List.LocalExtra.firstJustMap
                            (\variantDecoder ->
                                if variantDecoder.tag == tag then
                                    Just variantDecoder.value

                                else
                                    Nothing
                            )
                of
                    Just valueDecoder ->
                        Json.Decode.field "value" valueDecoder

                    Nothing ->
                        Json.Decode.fail
                            ("expected one of the following tags: "
                                ++ (variantDecoders
                                        |> List.map
                                            (\variantDecoder ->
                                                "\"" ++ variantDecoder.tag ++ "\""
                                            )
                                        |> String.join ", "
                                   )
                            )
            )


onlyString : String -> Json.Decode.Decoder ()
onlyString specificAllowedString =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                if str == specificAllowedString then
                    Json.Decode.succeed ()

                else
                    Json.Decode.fail
                        ("expected only \"" ++ specificAllowedString ++ "\"")
            )


resultOkErr : Json.Decode.Decoder a -> Json.Decode.Decoder a -> Json.Decode.Decoder a
resultOkErr okJsonDecoder errJsonDecoder =
    let
        valueOkJsonDecoder : Json.Decode.Decoder a
        valueOkJsonDecoder =
            Json.Decode.field "value" okJsonDecoder

        valueErrJsonDecoder : Json.Decode.Decoder a
        valueErrJsonDecoder =
            Json.Decode.field "value" errJsonDecoder
    in
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                case tag of
                    "Ok" ->
                        valueOkJsonDecoder

                    "Err" ->
                        valueErrJsonDecoder

                    _ ->
                        Json.Decode.fail "expected either \"Ok\" or \"Err\""
            )
