port module Main exposing (main)

{-| `mkdir _ && rm _`, demonstrating sub-process spawning and error handling
-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Dict
import Json.Encode
import Node


main : Node.Program State
main =
    Node.program
        { initialState = WaitingForDirectoryPathArgument
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = WaitingForDirectoryPathArgument
    | ArgumentsMalformed String
    | WaitingForMkdir { path : String, mkdirErrorOutput : String }
    | MkdirFailed { path : String, mkdirErrorOutput : String }
    | MkdirCompletedWaitingForRm { path : String, rmErrorOutput : String }
    | RmFailed { path : String, rmErrorOutput : String }
    | RmCompleted String


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForDirectoryPathArgument ->
            Node.launchArgumentsRequest
                |> Node.interfaceFutureMap
                    (\arguments ->
                        case arguments of
                            [ _, _, directoryPath ] ->
                                WaitingForMkdir
                                    { path = directoryPath
                                    , mkdirErrorOutput = ""
                                    }

                            _ :: _ :: _ :: _ :: _ ->
                                ArgumentsMalformed "Too many provided arguments. Please only specify a path to a directory to create."

                            _ ->
                                ArgumentsMalformed "The provided arguments are empty. Please specify a path to a directory to create."
                    )

        ArgumentsMalformed message ->
            [ Node.standardErrWrite (message ++ "\n")
            , Node.exit 1
            ]
                |> Node.interfaceBatch

        WaitingForMkdir waitingForMkdir ->
            Node.subProcessSpawn
                { command = "mkdir"
                , arguments = [ waitingForMkdir.path ]
                , environmentVariables = Dict.empty
                , workingDirectoryPath = ""
                , writeToStandardIn = Nothing
                }
                |> Node.interfaceFutureMap
                    (\mkdirEvent ->
                        case mkdirEvent of
                            Node.SubProcessExited 0 ->
                                MkdirCompletedWaitingForRm
                                    { path = waitingForMkdir.path
                                    , rmErrorOutput = ""
                                    }

                            Node.SubProcessExited _ ->
                                MkdirFailed waitingForMkdir

                            Node.SubProcessStandardOutEvent mkdirOutEvent ->
                                WaitingForMkdir
                                    { path = waitingForMkdir.path
                                    , mkdirErrorOutput =
                                        waitingForMkdir.mkdirErrorOutput
                                            ++ (mkdirOutEvent |> streamDataOfBytesToString)
                                    }

                            Node.SubProcessStandardErrEvent mkdirErrorEvent ->
                                WaitingForMkdir
                                    { path = waitingForMkdir.path
                                    , mkdirErrorOutput =
                                        waitingForMkdir.mkdirErrorOutput
                                            ++ (mkdirErrorEvent |> streamDataOfStringToString)
                                    }
                    )

        MkdirFailed mkdirFailed ->
            [ Node.standardErrWrite
                ("Could not create a directory at path "
                    ++ mkdirFailed.path
                    ++ ": "
                    ++ (mkdirFailed.mkdirErrorOutput |> String.trimRight)
                    ++ ".\n"
                )
            , Node.exit 1
            ]
                |> Node.interfaceBatch

        MkdirCompletedWaitingForRm mkdirCompletedWaitingForRm ->
            Node.subProcessSpawn
                { command = "rm"
                , arguments = [ "--dir", mkdirCompletedWaitingForRm.path ]
                , environmentVariables = Dict.empty
                , workingDirectoryPath = ""
                , writeToStandardIn = Nothing
                }
                |> Node.interfaceFutureMap
                    (\mkdirEvent ->
                        case mkdirEvent of
                            Node.SubProcessExited 0 ->
                                RmCompleted mkdirCompletedWaitingForRm.path

                            Node.SubProcessExited _ ->
                                RmFailed mkdirCompletedWaitingForRm

                            Node.SubProcessStandardOutEvent mkdirOutEvent ->
                                MkdirCompletedWaitingForRm
                                    { path = mkdirCompletedWaitingForRm.path
                                    , rmErrorOutput =
                                        mkdirCompletedWaitingForRm.rmErrorOutput
                                            ++ (mkdirOutEvent |> streamDataOfBytesToString)
                                    }

                            Node.SubProcessStandardErrEvent mkdirErrorEvent ->
                                MkdirCompletedWaitingForRm
                                    { path = mkdirCompletedWaitingForRm.path
                                    , rmErrorOutput =
                                        mkdirCompletedWaitingForRm.rmErrorOutput
                                            ++ (mkdirErrorEvent |> streamDataOfStringToString)
                                    }
                    )

        RmFailed rmFailed ->
            [ Node.standardErrWrite
                ("Could not remove the created directory at path "
                    ++ rmFailed.path
                    ++ ".\n"
                )
            , Node.exit 1
            ]
                |> Node.interfaceBatch

        RmCompleted path ->
            Node.standardOutWrite
                ("Created and removed the directory "
                    ++ path
                    ++ ".\n"
                )


streamDataOfBytesToString : Node.StreamReadEvent Bytes -> String
streamDataOfBytesToString streamReadEvent =
    case streamReadEvent of
        Node.StreamDataReceived chunk ->
            Bytes.Decode.decode (Bytes.Decode.string (Bytes.width chunk)) chunk
                |> Maybe.withDefault ""

        Node.StreamDataEndReached ->
            ""


streamDataOfStringToString : Node.StreamReadEvent String -> String
streamDataOfStringToString streamReadEvent =
    case streamReadEvent of
        Node.StreamDataReceived chunk ->
            chunk

        Node.StreamDataEndReached ->
            ""


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
