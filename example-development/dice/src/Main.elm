port module Main exposing (State(..), main)

import Color
import Json.Encode
import Random.Pcg.Extended
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
    WaitingForInitialRandomness


interface : State -> Web.Interface State
interface state =
    case state of
        WaitingForInitialRandomness ->
            Web.randomUnsignedInt32sRequest 4
                |> Web.interfaceFutureMap
                    (\unsignedInt32s ->
                        let
                            initialSeed : Random.Pcg.Extended.Seed
                            initialSeed =
                                Random.Pcg.Extended.initialSeed (unsignedInt32s |> List.head |> Maybe.withDefault 0) (unsignedInt32s |> List.drop 1)

                            ( diceEyes, newSeed ) =
                                Random.Pcg.Extended.step diceEyesRandomGenerator initialSeed
                        in
                        DiceUiState { diceEyes = diceEyes, seed = newSeed }
                    )

        DiceUiState randomStuff ->
            Web.domElement "div"
                [ Web.domStyle "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
                , Web.domStyle "color" (Color.rgb 1 1 1 |> Color.toCssString)
                , Web.domStyle "padding-left" "80px"
                , Web.domStyle "padding-right" "80px"
                , Web.domStyle "position" "fixed"
                , Web.domStyle "top" "0"
                , Web.domStyle "right" "0"
                , Web.domStyle "bottom" "0"
                , Web.domStyle "left" "0"
                ]
                [ Web.domElement "span"
                    [ Web.domStyle "font-size" "24em"
                    ]
                    [ randomStuff.diceEyes |> diceEyesToSymbol |> Web.domText ]
                , Web.domElement "br" [] []
                , buttonUi
                    [ Web.domStyle "font-size" "4em"
                    ]
                    [ Web.domText "roll the dice" ]
                    |> Web.domFutureMap (\() -> RerollClicked)
                ]
                |> Web.domRender
                |> Web.interfaceFutureMap
                    (\RerollClicked ->
                        let
                            ( diceEyes, newSeed ) =
                                Random.Pcg.Extended.step diceEyesRandomGenerator randomStuff.seed
                        in
                        DiceUiState { diceEyes = diceEyes, seed = newSeed }
                    )


diceEyesRandomGenerator : Random.Pcg.Extended.Generator Int
diceEyesRandomGenerator =
    Random.Pcg.Extended.int 1 6


buttonUi : List (Web.DomModifier ()) -> List (Web.DomNode ()) -> Web.DomNode ()
buttonUi modifiers subs =
    Web.domElement "button"
        ([ Web.domListenTo "click"
            |> Web.domModifierFutureMap (\_ -> ())
         , Web.domStyle "background-color" (Color.rgba 0 0 0 0 |> Color.toCssString)
         , Web.domStyle "border-top" "none"
         , Web.domStyle "border-left" "none"
         , Web.domStyle "border-right" "none"
         , Web.domStyle "border-bottom" ("5px solid " ++ (Color.rgba 1 1 1 0.5 |> Color.toCssString))
         , Web.domStyle "border-radius" "20px"
         , Web.domStyle "color" "inherit"
         , Web.domStyle "padding" "4px 13px"
         , Web.domStyle "margin" "0px 0px"
         ]
            ++ modifiers
        )
        subs


diceEyesToSymbol : Int -> String
diceEyesToSymbol diceEyes =
    case diceEyes of
        1 ->
            "⚀"

        2 ->
            "⚁"

        3 ->
            "⚂"

        4 ->
            "⚃"

        5 ->
            "⚄"

        _ ->
            "⚅"


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


type State
    = WaitingForInitialRandomness
    | DiceUiState { diceEyes : Int, seed : Random.Pcg.Extended.Seed }


type DiceUiEvent
    = RerollClicked
