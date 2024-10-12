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
    = WaitingForInitialTime
    | PreviousSecondIsKnown Time.Posix


initialState : State
initialState =
    WaitingForInitialTime


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForInitialTime ->
            Node.timePosixRequest
                |> Node.interfaceFutureMap
                    (\initialTime -> PreviousSecondIsKnown initialTime)

        PreviousSecondIsKnown lastSecondTime ->
            [ Node.consoleLog
                ((lastSecondTime |> Time.posixToMillis)
                    // 1000
                    |> String.fromInt
                )
            , Node.timePeriodicallyListen (Duration.seconds 1)
                |> Node.interfaceFutureMap
                    (\nextSecondTime -> PreviousSecondIsKnown nextSecondTime)
            ]
                |> Node.interfaceBatch


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
