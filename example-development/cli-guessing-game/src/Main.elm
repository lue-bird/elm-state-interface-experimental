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
    | Playing { secretNumber : Int, guesses : List Int }


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
                                    }
                    )

        Playing playing ->
            case playing.guesses of
                [] ->
                    [ Node.consoleLog
                        ("We're playing \"guess the number\". You have "
                            ++ (allowedGuessCount |> String.fromInt)
                            ++ " guesses to find out my secret number.\nYour first guess: >"
                        )
                    , Node.standardInListen
                        |> Node.interfaceFutureMap (\guess -> storeGuess guess playing)
                    ]
                        |> Node.interfaceBatch

                lastGuess :: guessesBeforeLast ->
                    let
                        guessCount : Int
                        guessCount =
                            (guessesBeforeLast |> List.length) + 1
                    in
                    if guessCount >= allowedGuessCount then
                        [ Node.consoleLog
                            ("⛔ That was your last guess. My secret number would have been "
                                ++ (playing.secretNumber |> String.fromInt)
                                ++ "."
                            )
                        , Node.exit 0
                        ]
                            |> Node.interfaceBatch

                    else if lastGuess == playing.secretNumber then
                        [ Node.consoleLog
                            ("🎊 Yes! My secret number was "
                                ++ (playing.secretNumber |> String.fromInt)
                                ++ "."
                            )
                        , Node.exit 0
                        ]
                            |> Node.interfaceBatch

                    else
                        [ Node.consoleLog
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
                                ++ " guess: >"
                            )
                        , Node.standardInListen
                            |> Node.interfaceFutureMap (\guess -> storeGuess guess playing)
                        ]
                            |> Node.interfaceBatch


storeGuess : String -> { secretNumber : Int, guesses : List Int } -> State
storeGuess guess playing =
    case guess |> String.trimRight |> String.toInt of
        Nothing ->
            Playing
                { secretNumber = playing.secretNumber
                , guesses = -1 :: playing.guesses
                }

        Just validNumberGuess ->
            Playing
                { secretNumber = playing.secretNumber
                , guesses = validNumberGuess :: playing.guesses
                }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
