module AsciiString exposing (fromBytes, toBytes)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode


toBytes : String -> Bytes
toBytes string =
    Bytes.Encode.encode (encoder string)


encoder : String -> Bytes.Encode.Encoder
encoder string =
    encodeChunks string []
        |> List.reverse
        |> Bytes.Encode.sequence


encodeChunks : String -> List Bytes.Encode.Encoder -> List Bytes.Encode.Encoder
encodeChunks input soFar =
    case String.toList (String.left 4 input) of
        [ a, b, c, d ] ->
            encodeChunks (String.dropLeft 4 input)
                (Bytes.Encode.unsignedInt32 Bytes.BE
                    (Bitwise.or
                        (Bitwise.or
                            (Bitwise.shiftLeftBy 24 (asciiCharToInt a))
                            (Bitwise.shiftLeftBy 16 (asciiCharToInt b))
                        )
                        (Bitwise.or
                            (Bitwise.shiftLeftBy 8 (asciiCharToInt c))
                            (asciiCharToInt d)
                        )
                    )
                    :: soFar
                )

        a :: _ ->
            encodeChunks (String.dropLeft 1 input)
                (Bytes.Encode.unsignedInt8
                    (asciiCharToInt a)
                    :: soFar
                )

        [] ->
            soFar


asciiCharToInt : Char -> Int
asciiCharToInt asciiChar =
    Char.toCode asciiChar



--


fromBytes : Bytes -> String
fromBytes bytes =
    Bytes.Decode.decode (decoder (Bytes.width bytes)) bytes
        |> Maybe.withDefault ""


decoder : Int -> Bytes.Decode.Decoder String
decoder width =
    Bytes.Decode.loop { remaining = width, string = "" } loopHelp


loopHelp : { remaining : Int, string : String } -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, string : String } String)
loopHelp soFar =
    if soFar.remaining >= 4 then
        Bytes.Decode.map
            (\chunk ->
                Bytes.Decode.Loop
                    { remaining = soFar.remaining - 4
                    , string =
                        soFar.string ++ unsignedInt32ToAsciiString chunk
                    }
            )
            decodeUnsignedInt32BE

    else if soFar.remaining == 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done soFar.string)

    else
        Bytes.Decode.map
            (\a ->
                Bytes.Decode.Loop
                    { remaining = soFar.remaining - 1
                    , string = soFar.string ++ unsignedInt8ToAsciiString a
                    }
            )
            Bytes.Decode.unsignedInt8


decodeUnsignedInt32BE : Bytes.Decode.Decoder Int
decodeUnsignedInt32BE =
    Bytes.Decode.unsignedInt32 Bytes.BE


unsignedInt32ToAsciiString : Int -> String
unsignedInt32ToAsciiString bits =
    unsignedInt8ToAsciiString (Bitwise.and (Bitwise.shiftRightZfBy 24 bits) 0xFF)
        ++ unsignedInt8ToAsciiString (Bitwise.and (Bitwise.shiftRightZfBy 16 bits) 0xFF)
        ++ unsignedInt8ToAsciiString (Bitwise.and (Bitwise.shiftRightZfBy 8 bits) 0xFF)
        ++ unsignedInt8ToAsciiString (Bitwise.and bits 0xFF)


unsignedInt8ToAsciiString : Int -> String
unsignedInt8ToAsciiString bits =
    String.fromChar (Char.fromCode bits)
