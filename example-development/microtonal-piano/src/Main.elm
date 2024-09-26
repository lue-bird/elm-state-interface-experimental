port module Main exposing (State(..), main)

import Color
import Json.Decode
import Json.Encode
import Time
import Web


main : Web.Program State
main =
    Web.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


initialState : State
initialState =
    State
        { windowSize = { width = 1920, height = 1080 } -- dummy
        , musicSource = Nothing
        , tonesPlaying = []
        , lastUnplayedClickPitchPercentage = Nothing
        }


interface : State -> Web.Interface State
interface =
    \(State state) ->
        [ [ Web.windowSizeRequest, Web.windowResizeListen ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap (\windowSize -> State { state | windowSize = windowSize })
        , case state.musicSource of
            Just (Err _) ->
                Web.consoleError "audio failed to load"

            Just (Ok musicSource) ->
                [ case state.lastUnplayedClickPitchPercentage of
                    Nothing ->
                        Web.interfaceNone

                    Just pitchPercentage ->
                        Web.timePosixRequest
                            |> Web.interfaceFutureMap
                                (\time ->
                                    State
                                        { state
                                            | tonesPlaying =
                                                state.tonesPlaying
                                                    |> (::) { time = time, pitchPercentage = pitchPercentage }
                                            , lastUnplayedClickPitchPercentage = Nothing
                                        }
                                )
                , state.tonesPlaying
                    |> List.map
                        (\tonePlaying ->
                            Web.audioFromSource musicSource tonePlaying.time
                                |> Web.audioVolumeScaleBy (Web.audioParameterAt 0.6)
                                |> Web.audioSpeedScaleBy
                                    (Web.audioParameterAt (2 ^ (((tonePlaying.pitchPercentage - 0.5) * 36) / 12)))
                                |> Web.audioPlay
                        )
                    |> Web.interfaceBatch
                ]
                    |> Web.interfaceBatch

            Nothing ->
                Web.audioSourceLoad "piano-C5.mp3"
                    |> Web.interfaceFutureMap
                        (\result -> State { state | musicSource = result |> Just })
        , Web.domElement "div"
            [ Web.domStyle "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
            , Web.domStyle "color" (Color.rgb 1 1 1 |> Color.toCssString)
            , Web.domStyle "position" "fixed"
            , Web.domStyle "top" "0"
            , Web.domStyle "right" "0"
            , Web.domStyle "bottom" "0"
            , Web.domStyle "left" "0"
            , Web.domListenTo "click"
                |> Web.domModifierFutureMap
                    (\clickJson ->
                        clickJson
                            |> Json.Decode.decodeValue
                                (Json.Decode.field "clientY" Json.Decode.float)
                    )
                |> Web.domModifierFutureMap
                    (\clientYResult ->
                        case clientYResult of
                            Err _ ->
                                State state

                            Ok clientY ->
                                State
                                    { state
                                        | lastUnplayedClickPitchPercentage =
                                            (1 - clientY / (state.windowSize.height |> Basics.toFloat)) |> Just
                                    }
                    )
            ]
            [ Web.domElement "table"
                [ Web.domStyle "width" "100%"
                , Web.domStyle "height" "100%"
                , Web.domStyle "position" "absolute"
                , Web.domStyle "z-index" "1"
                ]
                (List.range 0 35
                    |> List.map
                        (\i ->
                            Web.domElement "tr"
                                [ Web.domStyle "background"
                                    (Color.hsl
                                        ((i |> Basics.remainderBy 12 |> Basics.toFloat) / 12)
                                        1
                                        0.5
                                        |> Color.toCssString
                                    )
                                ]
                                [ Web.domElement "th"
                                    []
                                    []
                                ]
                        )
                )
            , Web.domElement "div"
                [ Web.domStyle "font-size" "3em"
                , Web.domStyle "padding" "1%"
                , Web.domStyle "margin" "auto"
                , Web.domStyle "width" "50%"
                , Web.domStyle "position" "relative"
                , Web.domStyle "user-select" "none"
                , Web.domStyle "text-align" "center"
                , Web.domStyle "z-index" "2"
                , Web.domStyle "background-color" (Color.rgba 0 0 0 0.5 |> Color.toCssString)
                ]
                [ Web.domElement "b" [] [ Web.domText "click height = note pitch" ]
                ]
            ]
            |> Web.domRender
        ]
            |> Web.interfaceBatch


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


type State
    = State
        { windowSize : { width : Int, height : Int }
        , musicSource : Maybe (Result Web.AudioSourceLoadError Web.AudioSource)
        , tonesPlaying : List { time : Time.Posix, pitchPercentage : Float }
        , lastUnplayedClickPitchPercentage : Maybe Float
        }
