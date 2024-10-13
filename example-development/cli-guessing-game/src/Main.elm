port module Main exposing (State(..), main)

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
    = WaitingForInitialRandomness
    | Playing StatePlaying


type alias StatePlaying =
    { secretNumber : Int
    , guesses : List Int
    , previousGuessInvalid : Bool
    }


initialState : State
initialState =
    WaitingForInitialRandomness


allowedGuessCount : Int
allowedGuessCount =
    6


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForInitialRandomness ->
            Node.randomUnsignedInt32sRequest 1
                |> Node.interfaceFutureMap
                    (\randomInts ->
                        case randomInts of
                            [] ->
                                WaitingForInitialRandomness

                            randomInt :: _ ->
                                Playing
                                    { secretNumber =
                                        randomInt |> Basics.abs |> Basics.remainderBy 100
                                    , guesses = []
                                    , previousGuessInvalid = False
                                    }
                    )

        Playing playing ->
            if playing.previousGuessInvalid then
                [ Node.standardOutWrite
                    ("Please write a number between 0 and 100. "
                        ++ (allowedGuessCount |> String.fromInt)
                        ++ " guesses remaining.\nYour guess: > "
                    )
                , Node.standardInListen
                    |> Node.interfaceFutureMap (\guess -> Playing (handleGuess guess playing))
                ]
                    |> Node.interfaceBatch

            else
                case playing.guesses of
                    [] ->
                        [ Node.standardOutWrite
                            ("We're playing \"guess the number\". You have "
                                ++ (allowedGuessCount |> String.fromInt)
                                ++ " guesses to find out my secret number.\nYour first guess: > "
                            )
                        , Node.standardInListen
                            |> Node.interfaceFutureMap (\guess -> Playing (handleGuess guess playing))
                        ]
                            |> Node.interfaceBatch

                    lastGuess :: guessesBeforeLast ->
                        let
                            guessCount : Int
                            guessCount =
                                (guessesBeforeLast |> List.length) + 1
                        in
                        if lastGuess == playing.secretNumber then
                            [ Node.consoleLog
                                ("🎊 Yes! My secret number was "
                                    ++ (playing.secretNumber |> String.fromInt)
                                    ++ "."
                                )
                            , Node.exit 0
                            ]
                                |> Node.interfaceBatch

                        else if guessCount >= allowedGuessCount then
                            [ Node.consoleLog
                                ("⛔ That was your last guess. My secret number would have been "
                                    ++ (playing.secretNumber |> String.fromInt)
                                    ++ "."
                                )
                            , Node.exit 0
                            ]
                                |> Node.interfaceBatch

                        else
                            [ Node.standardOutWrite
                                ((lastGuess |> String.fromInt)
                                    ++ " is "
                                    ++ (if lastGuess < playing.secretNumber then
                                            "less than"

                                        else
                                            "greater than"
                                       )
                                    ++ " my secret number. "
                                    ++ (allowedGuessCount - guessCount |> String.fromInt)
                                    ++ " guesses remaining."
                                    ++ "\nYour "
                                    ++ (if guessCount == allowedGuessCount then
                                            "last"

                                        else
                                            "next"
                                       )
                                    ++ " guess: > "
                                )
                            , Node.standardInListen
                                |> Node.interfaceFutureMap (\guess -> Playing (handleGuess guess playing))
                            ]
                                |> Node.interfaceBatch


handleGuess : String -> StatePlaying -> StatePlaying
handleGuess guess playing =
    case guess |> String.trimRight |> String.toInt of
        Nothing ->
            { secretNumber = playing.secretNumber
            , guesses = playing.guesses
            , previousGuessInvalid = True
            }

        Just numberGuess ->
            if numberGuess >= 0 && numberGuess <= 99 then
                { secretNumber = playing.secretNumber
                , guesses = numberGuess :: playing.guesses
                , previousGuessInvalid = False
                }

            else
                { secretNumber = playing.secretNumber
                , guesses = playing.guesses
                , previousGuessInvalid = True
                }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
