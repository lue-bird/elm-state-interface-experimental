port module Main exposing (main)

import Ansi.Color
import Ansi.Font
import Bytes
import Bytes.Decode
import Elm.Package
import Elm.Project
import Json.Decode
import Json.Encode
import Node


type State
    = WaitingForWorkingDirectory
    | Running RunningState
    | ElmJsonFileReadFailed String


type alias RunningState =
    { workingDirectory : String
    , elmJson : Maybe Elm.Project.Project
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
            runningInterface running

        ElmJsonFileReadFailed elmJsonDecodeError ->
            [ Node.standardErrWrite
                (elmJsonDecodeError ++ "\n")
            , Node.exit 1
            ]
                |> Node.interfaceBatch


runningInterface : RunningState -> Node.Interface State
runningInterface running =
    case running.elmJson of
        Nothing ->
            Node.fileRequest (running.workingDirectory ++ "/elm.json")
                |> Node.interfaceFutureMap
                    (\elmJsonBytesOrError ->
                        case elmJsonBytesOrError of
                            Err fileReadError ->
                                ElmJsonFileReadFailed
                                    ("elm.json couldn't be read because "
                                        ++ fileReadError.message
                                        ++ " (code "
                                        ++ fileReadError.code
                                        ++ ")"
                                    )

                            Ok elmJsonBytes ->
                                case elmJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width elmJsonBytes)) of
                                    Nothing ->
                                        ElmJsonFileReadFailed "elm.json bytes could not be decoded into UTF-8 String"

                                    Just elmJsonString ->
                                        case elmJsonString |> Json.Decode.decodeString Elm.Project.decoder of
                                            Err jsonDecodeError ->
                                                ElmJsonFileReadFailed
                                                    ("elm.json failed to parse due to "
                                                        ++ (jsonDecodeError |> Json.Decode.errorToString)
                                                    )

                                            Ok elmJson ->
                                                Running
                                                    { workingDirectory = running.workingDirectory
                                                    , elmJson = Just elmJson
                                                    }
                    )

        Just elmJson ->
            Node.standardOutWrite
                ("elm.json parsed successfully.\nThe "
                    ++ (case elmJson of
                            Elm.Project.Application application ->
                                "application has "
                                    ++ (application.depsDirect
                                            |> List.length
                                            |> String.fromInt
                                            |> Ansi.Font.bold
                                            |> Ansi.Color.fontColor Ansi.Color.magenta
                                       )
                                    ++ " direct dependencies"

                            Elm.Project.Package package ->
                                "package "
                                    ++ (package.name |> Elm.Package.toString)
                                    ++ " - "
                                    ++ package.summary
                                    ++ "\nhas "
                                    ++ (package.deps
                                            |> List.length
                                            |> String.fromInt
                                            |> Ansi.Font.bold
                                            |> Ansi.Color.fontColor Ansi.Color.magenta
                                       )
                                    ++ " direct dependencies"
                       )
                    ++ "\n"
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
