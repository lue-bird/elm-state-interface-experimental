module Json.Decode.LocalExtra exposing (onlyString, resultOkErr, variant)

import Json.Decode


variant : String -> (Json.Decode.Decoder value -> Json.Decode.Decoder value)
variant name valueJsonDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                if tag == name then
                    Json.Decode.field "value" valueJsonDecoder

                else
                    ("expected only \"" ++ name ++ "\"")
                        |> Json.Decode.fail
            )


onlyString : String -> Json.Decode.Decoder ()
onlyString specificAllowedString =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                if str == specificAllowedString then
                    () |> Json.Decode.succeed

                else
                    ("expected only \"" ++ specificAllowedString ++ "\"")
                        |> Json.Decode.fail
            )


resultOkErr : Json.Decode.Decoder a -> Json.Decode.Decoder a -> Json.Decode.Decoder a
resultOkErr errJsonDecoder okJsonDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                case tag of
                    "Ok" ->
                        okJsonDecoder

                    "Err" ->
                        errJsonDecoder

                    _ ->
                        Json.Decode.fail "expected either Ok or Err"
            )
