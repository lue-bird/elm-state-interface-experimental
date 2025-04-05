port module Main exposing (main)

{-| like `cat`
-}

import Bytes
import Bytes.Decode
import Json.Encode
import Node


main : Node.Program State
main =
    Node.program
        { initialState = WaitingForFilePathArgument
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = WaitingForFilePathArgument
    | ArgumentsMalformed String
    | WaitingForFileContent { filePath : String }
    | FileContentReceived String
    | FailedToReadFileContent { filePath : String, reason : String }


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForFilePathArgument ->
            Node.launchArgumentsRequest
                |> Node.interfaceFutureMap
                    (\arguments ->
                        case arguments of
                            [ _, _, filePath ] ->
                                WaitingForFileContent { filePath = filePath }

                            _ :: _ :: _ :: _ :: _ ->
                                ArgumentsMalformed "Too many provided arguments. Please only specify a path to a file."

                            _ ->
                                ArgumentsMalformed "The provided arguments are empty. Please specify a path to a file."
                    )

        WaitingForFileContent waitingForFileContent ->
            Node.fileRequest waitingForFileContent.filePath
                |> Node.interfaceFutureMap
                    (\fileContentBytesOrError ->
                        case fileContentBytesOrError of
                            Err fileReadError ->
                                FailedToReadFileContent
                                    { filePath = waitingForFileContent.filePath
                                    , reason = "the file could not be read: " ++ fileReadError.message
                                    }

                            Ok fileContentBytes ->
                                case fileContentBytes |> Bytes.Decode.decode (Bytes.Decode.string (fileContentBytes |> Bytes.width)) of
                                    Nothing ->
                                        FailedToReadFileContent
                                            { filePath = waitingForFileContent.filePath
                                            , reason = "the file content bytes could not be turned into a UTF-8 string"
                                            }

                                    Just fileContent ->
                                        FileContentReceived fileContent
                    )

        FileContentReceived fileContent ->
            Node.standardOutWrite fileContent

        ArgumentsMalformed message ->
            [ Node.standardErrWrite (message ++ "\n")
            , Node.exit 1
            ]
                |> Node.interfaceBatch

        FailedToReadFileContent failedToReadFileContent ->
            [ Node.standardErrWrite
                ("Could not read the file at path "
                    ++ failedToReadFileContent.filePath
                    ++ " because "
                    ++ failedToReadFileContent.reason
                    ++ "."
                )
            , Node.exit 1
            ]
                |> Node.interfaceBatch


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
