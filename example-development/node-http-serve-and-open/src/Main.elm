port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode
import Dict exposing (Dict)
import Json.Encode
import Node


main : Node.Program State
main =
    Node.program
        { initialState =
            StartingUp
                { servingHtml = False
                , environmentVariables = Nothing
                }
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = StartingUp
        { servingHtml : Bool
        , environmentVariables : Maybe (Dict String String)
        }
    | HttpServerFailed { code : String, message : String }


interface : State -> Node.Interface State
interface state =
    case state of
        StartingUp startingUp ->
            [ Node.standardOutWrite "server starting up\n"
            , Node.httpRequestListenAndMaybeRespond
                { portNumber = 4850
                , response =
                    Just
                        { statusCode = 200
                        , headers = [ { name = "Content-Type", value = "text/html; charset=utf-8" } ]
                        , data = indexHtml |> Bytes.Encode.string |> Bytes.Encode.encode
                        }
                }
                |> Node.interfaceFutureMap
                    (\event ->
                        case event of
                            Node.HttpServerOpened ->
                                StartingUp { startingUp | servingHtml = True }

                            Node.HttpRequestReceived _ ->
                                StartingUp startingUp

                            Node.HttpResponseSent ->
                                StartingUp startingUp

                            Node.HttpServerFailed error ->
                                HttpServerFailed error
                    )
            , Node.environmentVariablesRequest
                |> Node.interfaceFutureMap
                    (\environmentVariables ->
                        StartingUp
                            { startingUp
                                | environmentVariables = Just environmentVariables
                            }
                    )
            , case ( startingUp.servingHtml, startingUp.environmentVariables ) of
                ( True, Just environmentVariables ) ->
                    [ Node.standardOutWrite "serving at http://localhost:4850\n"
                    , Node.subProcessSpawn
                        { command = "open"
                        , arguments = [ "http://localhost:4850" ]
                        , workingDirectoryPath = ""
                        , environmentVariables = environmentVariables
                        , writeToStandardIn = Nothing
                        }
                        |> Node.interfaceFutureMap (\_ -> StartingUp startingUp)
                    ]
                        |> Node.interfaceBatch

                _ ->
                    Node.interfaceNone
            ]
                |> Node.interfaceBatch

        HttpServerFailed error ->
            [ Node.standardErrWrite
                ("HTTP server can't run because " ++ error.message ++ ".\n")
            , Node.exit 1
            ]
                |> Node.interfaceBatch


indexHtml : String
indexHtml =
    """<!DOCTYPE html>
<html>
  <head>
  <title>cool beans</title>
  </head>
  <body>
    <div>
      <h1>Hello, world!</h1>
      <p>Opening it in the browser from the command worked.</p>
    </div>
  </body>
</html>"""


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
