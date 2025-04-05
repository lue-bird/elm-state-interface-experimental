port module Main exposing (main)

{-| Like `open` but accepting multiple urls as arguments.

I often store some links in a text file, one on each line.
If you do the same, you can also use

    npm run start 'first-url
    second-url
    third-url
    ...'

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Json.Encode
import Node


main : Node.Program State
main =
    Node.program
        { initialState =
            Running
                { urls = Nothing
                , environmentVariables = Nothing
                , openFailures = Dict.empty
                }
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = Running
        { urls : Maybe (List String)
        , environmentVariables : Maybe (Dict String String)
        , openFailures : Dict String String
        }


interface : State -> Node.Interface State
interface state =
    case state of
        Running running ->
            [ Node.launchArgumentsRequest
                |> Node.interfaceFutureMap
                    (\launchArguments ->
                        Running
                            { running
                                | urls =
                                    Just
                                        (launchArguments
                                            |> List.drop 2
                                            |> List.concatMap String.lines
                                        )
                            }
                    )
            , Node.environmentVariablesRequest
                |> Node.interfaceFutureMap
                    (\environmentVariables ->
                        Running
                            { running
                                | environmentVariables = Just environmentVariables
                            }
                    )
            , case ( running.urls, running.environmentVariables ) of
                ( Just urls, Just environmentVariables ) ->
                    [ Node.standardOutWrite ("opening " ++ (urls |> String.join " ") ++ "\n")
                    , urls
                        |> List.map
                            (\urlToOpen ->
                                Node.subProcessSpawn
                                    { command = "open"
                                    , arguments = [ urlToOpen ]
                                    , workingDirectoryPath = ""
                                    , environmentVariables = environmentVariables
                                    , writeToStandardIn = Nothing
                                    }
                                    |> Node.interfaceFutureMap
                                        (\openEvent ->
                                            case openEvent of
                                                Node.SubProcessExited _ ->
                                                    Running running

                                                Node.SubProcessStandardOutEvent _ ->
                                                    Running running

                                                Node.SubProcessStandardErrEvent Node.StreamDataEndReached ->
                                                    Running running

                                                Node.SubProcessStandardErrEvent (Node.StreamDataReceived chunk) ->
                                                    Running
                                                        { running
                                                            | openFailures =
                                                                running.openFailures
                                                                    |> Dict.update urlToOpen
                                                                        (\existingFailureMessage ->
                                                                            Just
                                                                                ((existingFailureMessage |> Maybe.withDefault "")
                                                                                    ++ chunk
                                                                                )
                                                                        )
                                                        }
                                        )
                            )
                        |> Node.interfaceBatch
                    , running.openFailures
                        |> Dict.toList
                        |> List.map
                            (\( url, openFailure ) ->
                                Node.standardErrWrite
                                    ("Failed to open "
                                        ++ url
                                        ++ " because "
                                        ++ (openFailure |> String.trimRight)
                                        ++ ". Maybe you forgot to prepend https:// ?\n"
                                    )
                            )
                        |> Node.interfaceBatch
                    ]
                        |> Node.interfaceBatch

                _ ->
                    Node.interfaceNone
            ]
                |> Node.interfaceBatch


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
