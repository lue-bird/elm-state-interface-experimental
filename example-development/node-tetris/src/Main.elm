port module Main exposing (main)

{-| simplified tetris (no mirrored L and S for example)

  - left/right to move left
  - up to turn clockwise, down to turn counterclockwise

-}

import Ansi
import Ansi.Color
import Ansi.Cursor
import Ansi.Decode
import Ansi.Font
import Ansi.String
import Array exposing (Array)
import Duration
import Json.Encode
import Node
import Random


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


type State
    = Playing
        { ticks : Int
        , groundPieceCoordinates : Array (Array (Maybe PieceKind))
        , fallingPiece : { kind : PieceKind, x : Int, y : Int, turn : PieceTurn }
        }
    | GameOver { ticks : Int }


type PieceKind
    = Square
    | L
    | S
    | T
    | I


type PieceTurn
    = NoTurn
    | QuarterTurn
    | HalfTurn
    | ThreeQuartersTurn


pieceTurnByQuarterClockwise : PieceTurn -> PieceTurn
pieceTurnByQuarterClockwise pieceTurn =
    case pieceTurn of
        NoTurn ->
            QuarterTurn

        QuarterTurn ->
            HalfTurn

        HalfTurn ->
            ThreeQuartersTurn

        ThreeQuartersTurn ->
            NoTurn


pieceTurnByQuarterCounterclockwise : PieceTurn -> PieceTurn
pieceTurnByQuarterCounterclockwise pieceTurn =
    case pieceTurn of
        NoTurn ->
            ThreeQuartersTurn

        QuarterTurn ->
            NoTurn

        HalfTurn ->
            QuarterTurn

        ThreeQuartersTurn ->
            HalfTurn


interface : State -> Node.Interface State
interface state =
    case state of
        Playing playing ->
            [ Node.standardOutWrite
                (Ansi.clearScreen
                    ++ Ansi.Cursor.hide
                    ++ (playing.groundPieceCoordinates
                            |> fieldPlacePiece playing.fallingPiece
                            |> Array.map
                                (\line ->
                                    line |> Array.toList |> List.map maybePieceKindAsText |> String.concat
                                )
                            |> Array.toList
                            |> String.join "\n"
                            |> textBordered
                       )
                )
            , Node.standardInRawListen
                |> Node.interfaceFutureMap
                    (\inputEvent ->
                        case inputEvent of
                            Node.StreamDataEndReached ->
                                Playing playing

                            Node.StreamDataReceived input ->
                                if input |> Ansi.Decode.isLeftArrow then
                                    let
                                        fallingPieceAfterSuccessfulMove : { x : Int, y : Int, turn : PieceTurn, kind : PieceKind }
                                        fallingPieceAfterSuccessfulMove =
                                            { x = playing.fallingPiece.x - 1
                                            , y = playing.fallingPiece.y
                                            , turn = playing.fallingPiece.turn
                                            , kind = playing.fallingPiece.kind
                                            }
                                    in
                                    if
                                        fallingPieceCollidesWithGroundOrSides
                                            { groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }
                                    then
                                        Playing playing

                                    else
                                        Playing
                                            { ticks = playing.ticks
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            , groundPieceCoordinates = playing.groundPieceCoordinates
                                            }

                                else if input |> Ansi.Decode.isRightArrow then
                                    let
                                        fallingPieceAfterSuccessfulMove : { x : Int, y : Int, turn : PieceTurn, kind : PieceKind }
                                        fallingPieceAfterSuccessfulMove =
                                            { x = playing.fallingPiece.x + 1
                                            , y = playing.fallingPiece.y
                                            , turn = playing.fallingPiece.turn
                                            , kind = playing.fallingPiece.kind
                                            }
                                    in
                                    if
                                        fallingPieceCollidesWithGroundOrSides
                                            { groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }
                                    then
                                        Playing playing

                                    else
                                        Playing
                                            { ticks = playing.ticks
                                            , groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }

                                else if input |> Ansi.Decode.isUpArrow then
                                    let
                                        fallingPieceAfterSuccessfulMove : { x : Int, y : Int, turn : PieceTurn, kind : PieceKind }
                                        fallingPieceAfterSuccessfulMove =
                                            { x = playing.fallingPiece.x
                                            , y = playing.fallingPiece.y
                                            , turn = playing.fallingPiece.turn |> pieceTurnByQuarterClockwise
                                            , kind = playing.fallingPiece.kind
                                            }
                                    in
                                    if
                                        fallingPieceCollidesWithGroundOrSides
                                            { groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }
                                    then
                                        Playing playing

                                    else
                                        Playing
                                            { ticks = playing.ticks
                                            , groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }

                                else if input |> Ansi.Decode.isDownArrow then
                                    let
                                        fallingPieceAfterSuccessfulMove : { x : Int, y : Int, turn : PieceTurn, kind : PieceKind }
                                        fallingPieceAfterSuccessfulMove =
                                            { x = playing.fallingPiece.x
                                            , y = playing.fallingPiece.y
                                            , turn = playing.fallingPiece.turn |> pieceTurnByQuarterCounterclockwise
                                            , kind = playing.fallingPiece.kind
                                            }
                                    in
                                    if
                                        fallingPieceCollidesWithGroundOrSides
                                            { groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }
                                    then
                                        Playing playing

                                    else
                                        Playing
                                            { ticks = playing.ticks
                                            , groundPieceCoordinates = playing.groundPieceCoordinates
                                            , fallingPiece = fallingPieceAfterSuccessfulMove
                                            }

                                else
                                    Playing playing
                    )
            , Node.timePeriodicallyListen (Duration.seconds (0.05 + 0.3 * (20 + playing.ticks |> Basics.toFloat) ^ -0.2))
                |> Node.interfaceFutureMap
                    (\_ ->
                        let
                            fallingPieceAfterSuccessfulMove : { x : Int, y : Int, turn : PieceTurn, kind : PieceKind }
                            fallingPieceAfterSuccessfulMove =
                                { x = playing.fallingPiece.x
                                , y = playing.fallingPiece.y + 1
                                , turn = playing.fallingPiece.turn
                                , kind = playing.fallingPiece.kind
                                }
                        in
                        if
                            fallingPieceCollidesWithGroundOrSides
                                { groundPieceCoordinates = playing.groundPieceCoordinates
                                , fallingPiece = fallingPieceAfterSuccessfulMove
                                }
                        then
                            if playing.fallingPiece.y == 0 then
                                GameOver { ticks = playing.ticks }

                            else
                                Playing
                                    { ticks = playing.ticks + 1
                                    , fallingPiece =
                                        { kind =
                                            Random.step
                                                pieceKindRandom
                                                (Random.initialSeed playing.ticks)
                                                |> Tuple.first
                                        , x = fieldWidth // 2 - 1
                                        , y = 0
                                        , turn = NoTurn
                                        }
                                    , groundPieceCoordinates =
                                        playing.groundPieceCoordinates
                                            |> fieldPlacePiece playing.fallingPiece
                                            |> fieldRemoveFollRow
                                    }

                        else
                            Playing
                                { ticks = playing.ticks + 1
                                , fallingPiece = fallingPieceAfterSuccessfulMove
                                , groundPieceCoordinates = playing.groundPieceCoordinates
                                }
                    )
            ]
                |> Node.interfaceBatch

        GameOver gameOver ->
            Node.standardOutWrite
                ("\n\nGame over\nscore: "
                    ++ (gameOver.ticks |> String.fromInt |> Ansi.Font.bold)
                    ++ "\n\n\n"
                )


fallingPieceCollidesWithGroundOrSides :
    { fallingPiece : { kind : PieceKind, turn : PieceTurn, y : Int, x : Int }
    , groundPieceCoordinates : Array (Array (Maybe PieceKind))
    }
    -> Bool
fallingPieceCollidesWithGroundOrSides state =
    pieceKindCoordinates state.fallingPiece.kind state.fallingPiece.turn
        |> List.any
            (\c ->
                case
                    state.groundPieceCoordinates
                        |> Array.get (state.fallingPiece.y + c.y)
                        |> Maybe.andThen (Array.get (state.fallingPiece.x + c.x))
                of
                    Nothing ->
                        True

                    Just Nothing ->
                        False

                    Just (Just _) ->
                        True
            )


pieceKindRandom : Random.Generator PieceKind
pieceKindRandom =
    Random.uniform Square [ L, S, T, I ]


fieldHeight : Int
fieldHeight =
    20


fieldWidth : Int
fieldWidth =
    10


initialState : State
initialState =
    Playing
        { ticks = 0
        , fallingPiece = { kind = Square, x = 4, y = 3, turn = NoTurn }
        , groundPieceCoordinates =
            Array.repeat fieldHeight
                (Array.repeat fieldWidth Nothing)
        }


pieceKindColor : PieceKind -> Ansi.Color.Color
pieceKindColor blockKind =
    case blockKind of
        Square ->
            Ansi.Color.yellow

        L ->
            Ansi.Color.blue

        S ->
            Ansi.Color.green

        T ->
            Ansi.Color.magenta

        I ->
            Ansi.Color.red


maybePieceKindAsText : Maybe PieceKind -> String
maybePieceKindAsText maybePieceKind =
    case maybePieceKind of
        Nothing ->
            " ▾ "
                |> Ansi.Color.fontColor Ansi.Color.brightBlack

        Just pieceKind ->
            --"▁▁▁"
            "   "
                |> Ansi.Color.fontColor Ansi.Color.black
                |> Ansi.Color.backgroundColor (pieceKind |> pieceKindColor)


fieldPlacePiece :
    { kind : PieceKind, y : Int, x : Int, turn : PieceTurn }
    ->
        (Array (Array (Maybe PieceKind))
         -> Array (Array (Maybe PieceKind))
        )
fieldPlacePiece piece field =
    pieceKindCoordinates piece.kind piece.turn
        |> List.foldl
            (\relativeCoordinate fieldSoFar ->
                fieldSoFar
                    |> arrayAlterAt (piece.y + relativeCoordinate.y)
                        (Array.set (piece.x + relativeCoordinate.x)
                            (Just piece.kind)
                        )
            )
            field


fieldRemoveFollRow :
    Array (Array (Maybe PieceKind))
    -> Array (Array (Maybe PieceKind))
fieldRemoveFollRow field =
    let
        result =
            field
                |> Array.foldl
                    (\row soFar ->
                        if
                            row
                                |> arrayAny
                                    (\c ->
                                        case c of
                                            Nothing ->
                                                True

                                            _ ->
                                                False
                                    )
                        then
                            { removedRowCount = soFar.removedRowCount
                            , newRows = soFar.newRows |> Array.push row
                            }

                        else
                            -- all in row Just
                            { removedRowCount = soFar.removedRowCount + 1
                            , newRows = soFar.newRows
                            }
                    )
                    { removedRowCount = 0
                    , newRows = Array.empty
                    }
    in
    Array.append
        (Array.repeat result.removedRowCount
            (Array.repeat fieldWidth Nothing)
        )
        result.newRows


pieceKindCoordinates : PieceKind -> PieceTurn -> List { x : Int, y : Int }
pieceKindCoordinates pieceKind =
    -- https://eu2-browse.startpage.com/av/anon-image?piurl=https%3A%2F%2Fi.pinimg.com%2F736x%2F07%2Fbf%2Fd7%2F07bfd7e344183c428d841cf2813de97a.jpg&sp=1729085590T8ec77bc644150705a6f7b56f32a46732cce5de96a9d4b658389dfb1417c760bf
    case pieceKind of
        Square ->
            \_ ->
                [ { x = 0, y = 0 }
                , { x = 0, y = 1 }
                , { x = 1, y = 0 }
                , { x = 1, y = 1 }
                ]

        L ->
            \pieceTurn ->
                case pieceTurn of
                    NoTurn ->
                        [ { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 1 }
                        , { x = 2, y = 0 }
                        ]

                    QuarterTurn ->
                        [ { x = 1, y = 0 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 2 }
                        , { x = 2, y = 2 }
                        ]

                    HalfTurn ->
                        [ { x = 0, y = 2 }
                        , { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 1 }
                        ]

                    ThreeQuartersTurn ->
                        [ { x = 0, y = 0 }
                        , { x = 1, y = 0 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 2 }
                        ]

        I ->
            \pieceTurn ->
                case pieceTurn of
                    NoTurn ->
                        [ { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 1 }
                        , { x = 3, y = 1 }
                        ]

                    QuarterTurn ->
                        [ { x = 2, y = 0 }
                        , { x = 2, y = 1 }
                        , { x = 2, y = 2 }
                        , { x = 2, y = 3 }
                        ]

                    HalfTurn ->
                        [ { x = 0, y = 2 }
                        , { x = 1, y = 2 }
                        , { x = 2, y = 2 }
                        , { x = 3, y = 2 }
                        ]

                    ThreeQuartersTurn ->
                        [ { x = 1, y = 0 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 2 }
                        , { x = 1, y = 3 }
                        ]

        S ->
            \pieceTurn ->
                case pieceTurn of
                    NoTurn ->
                        [ { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 0 }
                        , { x = 2, y = 0 }
                        ]

                    QuarterTurn ->
                        [ { x = 1, y = 0 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 1 }
                        , { x = 2, y = 2 }
                        ]

                    HalfTurn ->
                        [ { x = 0, y = 2 }
                        , { x = 1, y = 2 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 1 }
                        ]

                    ThreeQuartersTurn ->
                        [ { x = 0, y = 0 }
                        , { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 2 }
                        ]

        T ->
            \pieceTurn ->
                case pieceTurn of
                    NoTurn ->
                        [ { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 0 }
                        , { x = 2, y = 1 }
                        ]

                    QuarterTurn ->
                        [ { x = 1, y = 0 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 1 }
                        , { x = 1, y = 2 }
                        ]

                    HalfTurn ->
                        [ { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 2 }
                        , { x = 2, y = 1 }
                        ]

                    ThreeQuartersTurn ->
                        [ { x = 1, y = 0 }
                        , { x = 0, y = 1 }
                        , { x = 1, y = 1 }
                        , { x = 1, y = 2 }
                        ]


textBordered : String -> String
textBordered text =
    let
        textLines : List String
        textLines =
            text |> String.lines

        textWidthCharCount : Int
        textWidthCharCount =
            textLines
                |> List.map (\line -> line |> Ansi.String.width)
                |> List.maximum
                |> Maybe.withDefault 0

        texture : String
        texture =
            --░ ▓ ▚
            " " |> Ansi.Color.fontColor Ansi.Color.brightBlack
    in
    (texture ++ texture ++ texture ++ String.repeat textWidthCharCount texture ++ texture ++ texture ++ texture)
        ++ "\n"
        ++ (textLines
                |> List.map
                    (\textLine ->
                        (texture ++ texture ++ texture)
                            ++ textLine
                            ++ (texture ++ texture ++ texture)
                    )
                |> String.join "\n"
           )
        ++ "\n"
        ++ (texture ++ texture ++ texture ++ String.repeat textWidthCharCount texture ++ texture ++ texture ++ texture)


arrayAlterAt : Int -> (a -> a) -> (Array a -> Array a)
arrayAlterAt index elementChange array =
    case array |> Array.get index of
        Nothing ->
            array

        Just existingElement ->
            array |> Array.set index (existingElement |> elementChange)


arrayAny : (a -> Bool) -> (Array a -> Bool)
arrayAny isNeedle array =
    array
        |> Array.foldl
            (\element soFar -> soFar || (element |> isNeedle))
            False


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
