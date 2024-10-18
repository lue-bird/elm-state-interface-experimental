port module Main exposing (State(..), main)

import Color
import Json.Decode
import Json.Encode
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
    State { movies = Nothing }


interface : State -> Web.Interface State
interface (State state) =
    [ case state.movies of
        Just _ ->
            Web.interfaceNone

        Nothing ->
            Web.httpGet
                { url = "https://api.quotable.io/quotes/random"
                , expect =
                    Web.httpExpectJson
                        (Json.Decode.index 0
                            (Json.Decode.map2 (\content author -> { content = content, author = author })
                                (Json.Decode.field "content" Json.Decode.string)
                                (Json.Decode.field "author" Json.Decode.string)
                            )
                        )
                }
                |> Web.httpRequest
                |> Web.interfaceFutureMap MoviesReceived
    , Web.domElement "div"
        [ Web.domStyle "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
        , Web.domStyle "color" (Color.rgb 1 1 1 |> Color.toCssString)
        , Web.domStyle "font-size" "2em"
        , Web.domStyle "padding-left" "80px"
        , Web.domStyle "padding-right" "80px"
        , Web.domStyle "position" "fixed"
        , Web.domStyle "top" "0"
        , Web.domStyle "right" "0"
        , Web.domStyle "bottom" "0"
        , Web.domStyle "left" "0"
        ]
        [ Web.domElement "div"
            [ Web.domStyle "max-width" "870px"
            , Web.domStyle "padding-top" "80px"
            ]
            [ Web.domElement "h1" [] [ Web.domText "random quote" ]
            , Web.domElement "div"
                []
                [ Web.domElement "div"
                    [ Web.domStyle "font-size" "1.2em"
                    ]
                    [ case state.movies of
                        Nothing ->
                            Web.domText "waiting for response"

                        Just (Err Web.HttpBadUrl) ->
                            Web.domText "Malformed URL"

                        Just (Err Web.HttpNetworkError) ->
                            Web.domText "Network error"

                        Just (Err (Web.HttpBadStatus _)) ->
                            Web.domText "Bad response status"

                        Just (Ok (Err decodeError)) ->
                            Web.domText
                                (decodeError.jsonError |> Json.Decode.errorToString)

                        Just (Ok (Ok movies)) ->
                            Web.domElement "div"
                                []
                                [ Web.domElement "blockquote" [ Web.domStyle "textAlign" "center" ] [ Web.domText movies.content ]
                                , Web.domElement "i" [ Web.domStyle "textAlign" "center" ] [ Web.domText ("by " ++ movies.author) ]
                                ]
                    ]
                , Web.domElement "br" [] []
                , buttonUi
                    [ Web.domStyle "font-size" "2em"
                    ]
                    [ Web.domText "next!" ]
                    |> Web.domFutureMap (\() -> MoreClicked)
                ]
            ]
        ]
        |> Web.domRender
    ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (\event ->
                case event of
                    MoreClicked ->
                        State { state | movies = Nothing }

                    MoviesReceived moviesResponse ->
                        State { state | movies = moviesResponse |> Just }
            )


buttonUi : List (Web.DomModifier ()) -> List (Web.DomNode ()) -> Web.DomNode ()
buttonUi modifiers subs =
    Web.domElement "button"
        ([ Web.domListenTo "click"
            |> Web.domModifierFutureMap (\_ -> ())
         , Web.domStyle "background-color" (Color.rgba 0 0 0 0 |> Color.toCssString)
         , Web.domStyle "color" (Color.rgb 1 1 1 |> Color.toCssString)
         , Web.domStyle "padding" "4px 13px"
         , Web.domStyle "text-align" "center"
         , Web.domStyle "border-radius" "20px"
         , Web.domStyle "border-top" "none"
         , Web.domStyle "border-left" "none"
         , Web.domStyle "border-right" "none"
         , Web.domStyle "border-bottom" ("5px solid " ++ (Color.rgba 1 1 1 0.2 |> Color.toCssString))
         ]
            ++ modifiers
        )
        subs


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


type State
    = State
        { movies :
            Maybe
                (Result
                    Web.HttpError
                    (Result
                        { actualBody : String, jsonError : Json.Decode.Error }
                        { content : String, author : String }
                    )
                )
        }


type DiceUiEvent
    = MoreClicked
    | MoviesReceived
        (Result
            Web.HttpError
            (Result
                { actualBody : String, jsonError : Json.Decode.Error }
                { content : String, author : String }
            )
        )
