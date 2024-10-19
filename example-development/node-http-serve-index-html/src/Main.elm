port module Main exposing (State(..), main)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
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
    = Running RunningState
    | InitialIndexHtmlFileReadFailed { code : String, message : String }
    | HttpServerFailed { code : String, message : String }


type alias RunningState =
    { indexHtml : Maybe { source : Bytes, isOutOfDate : Bool }
    }


initialState : State
initialState =
    Running
        { indexHtml = Nothing
        }


interface : State -> Node.Interface State
interface state =
    case state of
        Running running ->
            runningInterface running

        InitialIndexHtmlFileReadFailed error ->
            [ Node.standardOutWrite
                ("HTTP server can't start as index.html couldn't be read because "
                    ++ error.message
                    ++ " (code "
                    ++ error.code
                    ++ ")"
                    ++ "\n"
                )
            , Node.exit 1
            ]
                |> Node.interfaceBatch

        HttpServerFailed error ->
            [ Node.standardOutWrite
                ("HTTP server can't run because "
                    ++ error.message
                    ++ " (code "
                    ++ error.code
                    ++ ")"
                    ++ "\n"
                )
            , Node.exit 1
            ]
                |> Node.interfaceBatch


runningInterface : RunningState -> Node.Interface State
runningInterface running =
    case running.indexHtml of
        Just indexHtml ->
            [ Node.httpRequestListenAndMaybeRespond
                { portNumber = 4850
                , response =
                    Just
                        { statusCode = 200
                        , headers = [ { name = "Content-Type", value = "text/html; charset=utf-8" } ]
                        , data = indexHtml.source
                        }
                }
                |> Node.interfaceFutureMap
                    (\event ->
                        case event of
                            Node.HttpServerOpened ->
                                Running running

                            Node.HttpRequestReceived _ ->
                                Running running

                            Node.HttpResponseSent ->
                                Running running

                            Node.HttpServerFailed error ->
                                HttpServerFailed error
                    )
            , Node.standardOutWrite "Open http://localhost:4850\n"
            , Node.fileChangeListen "index.html"
                |> Node.interfaceFutureMap
                    (\indexHtmlFileChange ->
                        case indexHtmlFileChange of
                            Node.FileRemoved _ ->
                                -- we could for example store and display the error
                                Running
                                    { indexHtml =
                                        Just { source = indexHtml.source, isOutOfDate = False }
                                    }

                            Node.FileAddedOrChanged _ ->
                                Running
                                    { indexHtml =
                                        Just { source = indexHtml.source, isOutOfDate = True }
                                    }
                    )
            , if indexHtml.isOutOfDate then
                Node.fileRequest "index.html"
                    |> Node.interfaceFutureMap
                        (\newIndexHtmlSourceOrError ->
                            case newIndexHtmlSourceOrError of
                                Ok newIndexHtmlSourceBytes ->
                                    Running
                                        { indexHtml =
                                            Just { source = newIndexHtmlSourceBytes, isOutOfDate = False }
                                        }

                                Err _ ->
                                    -- keep serving the old html until a new one gets created
                                    Running
                                        { indexHtml =
                                            Just { source = indexHtml.source, isOutOfDate = False }
                                        }
                        )

              else
                Node.interfaceNone
            ]
                |> Node.interfaceBatch

        Nothing ->
            Node.fileRequest "index.html"
                |> Node.interfaceFutureMap
                    (\newIndexHtmlSourceOrError ->
                        case newIndexHtmlSourceOrError of
                            Ok newIndexHtmlSourceBytes ->
                                Running
                                    { indexHtml =
                                        Just { source = newIndexHtmlSourceBytes, isOutOfDate = False }
                                    }

                            Err indexHtmlFileReadError ->
                                InitialIndexHtmlFileReadFailed indexHtmlFileReadError
                    )


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
