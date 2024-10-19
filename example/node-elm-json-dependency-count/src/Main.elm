port module Main exposing (State(..), main)

import Ansi.Color
import Ansi.Cursor
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
    | Running
        { workingDirectory : String
        , elmJson : Maybe (Result String Elm.Project.Project)
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
                    Node.fileRequest (running.workingDirectory ++ "/elm.json")
                        |> Node.interfaceFutureMap
                            (\elmJsonBytes ->
                                Running
                                    { workingDirectory = running.workingDirectory
                                    , elmJson =
                                        Just
                                            (case elmJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width elmJsonBytes)) of
                                                Nothing ->
                                                    Err "bytes could not be decoded into UTF-8 String"

                                                Just elmJsonString ->
                                                    case elmJsonString |> Json.Decode.decodeString Elm.Project.decoder of
                                                        Ok elmJson ->
                                                            Ok elmJson

                                                        Err jsonDecodeError ->
                                                            Err (jsonDecodeError |> Json.Decode.errorToString)
                                            )
                                    }
                            )

                Just (Ok elmJson) ->
                    Node.standardOutWrite
                        ("elm.json parsed successfully.\nThe "
                            ++ (case elmJson of
                                    Elm.Project.Application application ->
                                        "application has " ++ (application.depsDirect |> List.length |> String.fromInt |> Ansi.Font.bold |> Ansi.Color.fontColor Ansi.Color.magenta) ++ " direct dependencies"

                                    Elm.Project.Package package ->
                                        "package "
                                            ++ (package.name |> Elm.Package.toString)
                                            ++ " - "
                                            ++ package.summary
                                            ++ "\nhas "
                                            ++ (package.deps |> List.length |> String.fromInt |> Ansi.Font.bold |> Ansi.Color.fontColor Ansi.Color.magenta)
                                            ++ " direct dependencies"
                               )
                            ++ "\n"
                        )

                Just (Err elmJsonDecodeError) ->
                    Node.standardErrWrite
                        ("elm.json failed to parse due to "
                            ++ elmJsonDecodeError
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
