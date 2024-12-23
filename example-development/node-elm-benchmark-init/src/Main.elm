port module Main exposing (main)

{-| Copy a simple [benchmarks](https://dark.elm.dmy.fr/packages/elm-explorations/benchmark/latest/)
project template to the current working directory
-}

import Bytes.Encode
import Json.Encode
import Node
import Web


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = State
        { benchmarkDirectory : OperationState
        , elmJson : OperationState
        , srcDirectory : OperationState
        , benchmarksElm : OperationState
        , webMainElm : OperationState
        }


type OperationState
    = NotStarted
    | Result (Result { code : String, message : String } ())


initialState : State
initialState =
    State
        { benchmarkDirectory = NotStarted
        , elmJson = NotStarted
        , srcDirectory = NotStarted
        , benchmarksElm = NotStarted
        , webMainElm = NotStarted
        }


benchmarkDirectoryName : String
benchmarkDirectoryName =
    "benchmarks"


interface : State -> Node.Interface State
interface (State state) =
    [ case ( state.webMainElm, state.benchmarksElm, state.elmJson ) of
        ( Result (Ok ()), Result (Ok ()), Result (Ok ()) ) ->
            Node.standardOutWrite
                ("""
    """
                    ++ benchmarkDirectoryName
                    ++ """/
      ┣ elm.json
      ┗ src/
          ┣ Benchmarks.elm
          ┗ WebMain.elm

Add benchmarks in """
                    ++ benchmarkDirectoryName
                    ++ """/src/Benchmarks.elm
Run with `cd """
                    ++ benchmarkDirectoryName
                    ++ """ && elm reactor`, then open http://localhost:8000/src/WebMain.elm
"""
                )

        _ ->
            Node.interfaceNone
    , case state.benchmarkDirectory of
        NotStarted ->
            Node.directoryMake benchmarkDirectoryName
                |> Node.interfaceFutureMap
                    (\result -> State { state | benchmarkDirectory = Result result })

        Result (Err benchmarkDirectoryError) ->
            fileCreateErrorInterface benchmarkDirectoryError

        Result (Ok benchmarkDirectoryResult) ->
            [ case state.elmJson of
                NotStarted ->
                    Node.fileWrite
                        { path = benchmarkDirectoryName ++ "/elm.json"
                        , content = initElmJsonSource |> Bytes.Encode.string |> Bytes.Encode.encode
                        }
                        |> Node.interfaceFutureMap
                            (\result -> State { state | elmJson = Result result })

                Result (Err elmJsonCreateError) ->
                    fileCreateErrorInterface elmJsonCreateError

                Result (Ok ()) ->
                    Node.interfaceNone
            , case state.srcDirectory of
                NotStarted ->
                    Node.directoryMake (benchmarkDirectoryName ++ "/src")
                        |> Node.interfaceFutureMap
                            (\result -> State { state | srcDirectory = Result result })

                Result (Err srcDirectoryError) ->
                    fileCreateErrorInterface srcDirectoryError

                Result (Ok ()) ->
                    [ case state.webMainElm of
                        NotStarted ->
                            Node.fileWrite
                                { path = benchmarkDirectoryName ++ "/src/WebMain.elm"
                                , content = initWebMainElmSource |> Bytes.Encode.string |> Bytes.Encode.encode
                                }
                                |> Node.interfaceFutureMap
                                    (\result -> State { state | webMainElm = Result result })

                        Result (Err elmJsonCreateError) ->
                            fileCreateErrorInterface elmJsonCreateError

                        Result (Ok ()) ->
                            Node.interfaceNone
                    , case state.benchmarksElm of
                        NotStarted ->
                            Node.fileWrite
                                { path = benchmarkDirectoryName ++ "/src/Benchmarks.elm"
                                , content = initBenchmarksElmSource |> Bytes.Encode.string |> Bytes.Encode.encode
                                }
                                |> Node.interfaceFutureMap
                                    (\result -> State { state | benchmarksElm = Result result })

                        Result (Err elmJsonCreateError) ->
                            fileCreateErrorInterface elmJsonCreateError

                        Result (Ok ()) ->
                            Node.interfaceNone
                    ]
                        |> Node.interfaceBatch
            ]
                |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


fileCreateErrorInterface : { message : String, code : String } -> Node.Interface future_
fileCreateErrorInterface error =
    [ Node.standardErrWrite
        ("failed to create "
            ++ (benchmarkDirectoryName
                    ++ "/ due to "
                    ++ error.message
                    ++ " (code "
                    ++ error.code
                    ++ ")"
               )
        )
    , Node.exit 1
    ]
        |> Node.interfaceBatch


initElmJsonSource : String
initElmJsonSource =
    """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            "elm-explorations/benchmark": "1.0.2",
            "lue-bird/elm-alternative-benchmark-runner": "1.0.0"
        },
        "indirect": {
            "BrianHicks/elm-trend": "2.1.3",
            "avh4/elm-color": "1.0.0",
            "elm/browser": "1.0.2",
            "elm/html": "1.0.0",
            "elm/json": "1.1.3",
            "elm/regex": "1.0.0",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3",
            "mdgriffith/elm-ui": "1.1.8",
            "mdgriffith/style-elements": "5.0.2",
            "miniBill/elm-ui-with-context": "1.1.0",
            "robinheghan/murmur3": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""


initBenchmarksElmSource : String
initBenchmarksElmSource =
    """module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "TODO"
        []
"""


initWebMainElmSource : String
initWebMainElmSource =
    """module WebMain exposing (main)

import Benchmark.Runner.Alternative
import Benchmarks


main : Benchmark.Runner.Alternative.Program
main =
    Benchmark.Runner.Alternative.program Benchmarks.benchmarks
"""


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
