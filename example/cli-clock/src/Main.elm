port module Main exposing (State(..), main)

import Ansi
import Ansi.Color
import Ansi.Cursor
import Ansi.Decode
import Duration
import Json.Encode
import Node
import Time


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = State
        { lastSecondTime : Maybe Time.Posix
        , timeZone : Maybe Time.Zone
        , displayTimeZone : DisplayTimeZone
        }


type DisplayTimeZone
    = DisplayUtc
    | DisplayLocalTime


initialState : State
initialState =
    State
        { lastSecondTime = Nothing
        , timeZone = Nothing
        , displayTimeZone = DisplayLocalTime
        }


interface : State -> Node.Interface State
interface (State state) =
    case ( state.lastSecondTime, state.timeZone ) of
        ( Just lastSecondTime, Just timeZone ) ->
            let
                timeZoneToDisplay : Time.Zone
                timeZoneToDisplay =
                    case state.displayTimeZone of
                        DisplayUtc ->
                            Time.utc

                        DisplayLocalTime ->
                            timeZone
            in
            [ Node.standardOutWrite
                (Ansi.clearScreen
                    ++ Ansi.Cursor.hide
                    ++ (((lastSecondTime |> Time.toHour timeZoneToDisplay |> doubleDigitFormat)
                            ++ ":"
                            ++ (lastSecondTime |> Time.toMinute timeZoneToDisplay |> doubleDigitFormat)
                            ++ ":"
                            ++ (lastSecondTime |> Time.toSecond timeZoneToDisplay |> doubleDigitFormat)
                            |> textPadHorizontally 1
                            |> textBorderedRounded
                        )
                            ++ "\n\n"
                            ++ ("local"
                                    |> textPadHorizontally 1
                                    |> (case state.displayTimeZone of
                                            DisplayUtc ->
                                                Basics.identity

                                            DisplayLocalTime ->
                                                ansiHighlight
                                       )
                               )
                            ++ " "
                            ++ ("utc"
                                    |> textPadHorizontally 1
                                    |> (case state.displayTimeZone of
                                            DisplayUtc ->
                                                ansiHighlight

                                            DisplayLocalTime ->
                                                Basics.identity
                                       )
                               )
                       )
                )
            , Node.standardInRawListen
                |> Node.interfaceFutureMap
                    (\input ->
                        if input |> Ansi.Decode.isLeftArrow then
                            State
                                { lastSecondTime = Just lastSecondTime
                                , timeZone = Just timeZone
                                , displayTimeZone =
                                    case state.displayTimeZone of
                                        DisplayUtc ->
                                            DisplayLocalTime

                                        DisplayLocalTime ->
                                            DisplayUtc
                                }

                        else if input |> Ansi.Decode.isRightArrow then
                            State
                                { lastSecondTime = Just lastSecondTime
                                , timeZone = Just timeZone
                                , displayTimeZone =
                                    case state.displayTimeZone of
                                        DisplayUtc ->
                                            DisplayLocalTime

                                        DisplayLocalTime ->
                                            DisplayUtc
                                }

                        else
                            State state
                    )
            , Node.timePeriodicallyListen (Duration.seconds 1)
                |> Node.interfaceFutureMap
                    (\nextSecondTime ->
                        State
                            { lastSecondTime = Just nextSecondTime
                            , timeZone = Just timeZone
                            , displayTimeZone = state.displayTimeZone
                            }
                    )
            ]
                |> Node.interfaceBatch

        ( maybeLastSecondTime, maybeTimeZone ) ->
            [ case maybeLastSecondTime of
                Just _ ->
                    Node.interfaceNone

                Nothing ->
                    Node.timePosixRequest
                        |> Node.interfaceFutureMap
                            (\initialTime ->
                                State
                                    { lastSecondTime = Just initialTime
                                    , timeZone = maybeTimeZone
                                    , displayTimeZone = state.displayTimeZone
                                    }
                            )
            , case maybeTimeZone of
                Just _ ->
                    Node.interfaceNone

                Nothing ->
                    Node.timeZoneRequest
                        |> Node.interfaceFutureMap
                            (\timeZone ->
                                State
                                    { timeZone = Just timeZone
                                    , lastSecondTime = maybeLastSecondTime
                                    , displayTimeZone = state.displayTimeZone
                                    }
                            )
            ]
                |> Node.interfaceBatch


textBorderedRounded : String -> String
textBorderedRounded text =
    let
        textLines =
            text |> String.lines

        textWidthCharCount =
            textLines |> List.map String.length |> List.maximum |> Maybe.withDefault 0
    in
    ("╭" ++ String.repeat textWidthCharCount "─" ++ "╮\n")
        ++ (textLines
                |> List.map (\textLine -> "│" ++ textLine ++ "│")
                |> String.join "\n"
           )
        ++ "\n╰"
        ++ String.repeat textWidthCharCount "─"
        ++ "╯"


textPadHorizontally : Int -> (String -> String)
textPadHorizontally padCharCount text =
    String.repeat padCharCount " "
        ++ text
        ++ String.repeat padCharCount " "


ansiHighlight : String -> String
ansiHighlight text =
    text
        |> Ansi.Color.backgroundColor Ansi.Color.brightCyan
        |> Ansi.Color.fontColor Ansi.Color.black


doubleDigitFormat : Int -> String
doubleDigitFormat number =
    number |> String.fromInt |> String.padLeft 2 '0'


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
