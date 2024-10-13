port module Main exposing (State(..), main)

import Ansi.Color
import Ansi.Font
import Elm.Package
import Elm.Project
import Json.Decode
import Json.Encode
import Node


type State
    = WaitingForWorkingDirectory
    | Running
        { workingDirectory : String
        , elmJson : Maybe (Result Json.Decode.Error Elm.Project.Project)
        }


initialState : State
initialState =
    WaitingForWorkingDirectory


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForWorkingDirectory ->
            Node.workingDirectoryPathRequest
                |> Node.interfaceFutureMap
                    (\wd ->
                        Running
                            { workingDirectory = wd
                            , elmJson = Nothing
                            }
                    )

        Running running ->
            case running.elmJson of
                Nothing ->
                    Node.fileUtf8Request (running.workingDirectory ++ "/elm.json")
                        |> Node.interfaceFutureMap
                            (\elmJsonRaw ->
                                Running
                                    { workingDirectory = running.workingDirectory
                                    , elmJson =
                                        Just (elmJsonRaw |> Json.Decode.decodeString Elm.Project.decoder)
                                    }
                            )

                Just (Ok elmJson) ->
                    Node.consoleLog
                        ("elm.json parsed successfully.\nThe "
                            ++ (case elmJson of
                                    Elm.Project.Application application ->
                                        "application has " ++ (application.depsDirect |> List.length |> String.fromInt |> Ansi.Font.bold |> Ansi.Color.fontColor Ansi.Color.magenta) ++ " direct dependencies"

                                    Elm.Project.Package package ->
                                        "package "
                                            ++ (package.name |> Elm.Package.toString)
                                            ++ " – "
                                            ++ package.summary
                                            ++ "\nhas "
                                            ++ (package.deps |> List.length |> String.fromInt |> Ansi.Font.bold |> Ansi.Color.fontColor Ansi.Color.magenta)
                                            ++ " direct dependencies"
                               )
                        )

                Just (Err elmJsonDecodeError) ->
                    Node.consoleError
                        ("elm.json failed to parse due to "
                            ++ (elmJsonDecodeError |> Json.Decode.errorToString)
                        )


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
