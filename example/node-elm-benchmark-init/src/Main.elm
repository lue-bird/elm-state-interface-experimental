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
    = State
        { benchmarkDirectory : OperationState
        , srcDirectory : OperationState
        }


type OperationState
    = NotStarted
    | Result (Result { code : String, message : String } ())


initialState : State
initialState =
    State { benchmarkDirectory = NotStarted, srcDirectory = NotStarted }


benchmarkDirectoryName : String
benchmarkDirectoryName =
    "benchmarks"


interface : State -> Node.Interface State
interface (State state) =
    case ( state.benchmarkDirectory, state.srcDirectory ) of
        ( Result benchmarkDirectoryResult, Result srcDirectoryResult ) ->
            case ( benchmarkDirectoryResult, srcDirectoryResult ) of
                ( Ok (), Ok () ) ->
                    [ Node.fileUtf8Write
                        { path = benchmarkDirectoryName ++ "/elm.json"
                        , content = initElmJsonSource
                        }
                    , Node.fileUtf8Write
                        { path = benchmarkDirectoryName ++ "/src/WebMain.elm"
                        , content = initWebMainElmSource
                        }
                    , Node.fileUtf8Write
                        { path = benchmarkDirectoryName ++ "/src/Benchmarks.elm"
                        , content = initBenchmarksElmSource
                        }
                    , Node.standardOutWrite
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
Run with  cd """
                            ++ benchmarkDirectoryName
                            ++ """ && elm reactor , then open http://localhost:8000/src/WebMain.elm
"""
                        )
                    ]
                        |> Node.interfaceBatch

                ( benchmarkDirectoryResultMaybeError, srcDirectoryResultMaybeError ) ->
                    [ Node.standardErrWrite
                        ("failed to create "
                            ++ ([ benchmarkDirectoryResultMaybeError, srcDirectoryResultMaybeError ]
                                    |> List.filterMap
                                        (\result ->
                                            case result of
                                                Ok () ->
                                                    Nothing

                                                Err error ->
                                                    Just error
                                        )
                                    |> List.map
                                        (\error ->
                                            benchmarkDirectoryName
                                                ++ "/ due to "
                                                ++ error.message
                                                ++ " (code "
                                                ++ error.code
                                                ++ ")"
                                        )
                                    |> String.join " and "
                               )
                        )
                    , Node.exit 1
                    ]
                        |> Node.interfaceBatch

        ( benchmarkDirectory, srcDirectory ) ->
            [ case benchmarkDirectory of
                Result _ ->
                    Node.interfaceNone

                NotStarted ->
                    Node.directoryMake benchmarkDirectoryName
                        |> Node.interfaceFutureMap
                            (\result ->
                                State
                                    { benchmarkDirectory = Result result
                                    , srcDirectory = state.srcDirectory
                                    }
                            )
            , case srcDirectory of
                Result _ ->
                    Node.interfaceNone

                NotStarted ->
                    Node.directoryMake (benchmarkDirectoryName ++ "/src")
                        |> Node.interfaceFutureMap
                            (\result ->
                                State
                                    { benchmarkDirectory = state.benchmarkDirectory
                                    , srcDirectory = Result result
                                    }
                            )
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
