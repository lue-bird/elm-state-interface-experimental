port module Main exposing (State(..), main)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
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
    State { quotes = Nothing }


interface : State -> Web.Interface State
interface (State state) =
    [ case state.quotes of
        Just _ ->
            Web.interfaceNone

        Nothing ->
            Web.httpRequestSend
                { url = "https://dummyjson.com/quotes/random"
                , method = "GET"
                , headers = []
                , body = Nothing
                }
                |> Web.interfaceFutureMap QuotesReceived
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
                    [ case state.quotes of
                        Nothing ->
                            Web.domText "waiting for response"

                        Just (Err error) ->
                            Web.domText error

                        Just (Ok quotes) ->
                            Web.domElement "div"
                                []
                                [ Web.domElement "blockquote" [ Web.domStyle "textAlign" "center" ] [ Web.domText quotes.content ]
                                , Web.domElement "i" [ Web.domStyle "textAlign" "center" ] [ Web.domText ("by " ++ quotes.author) ]
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
                        State { state | quotes = Nothing }

                    QuotesReceived responseBytesOrError ->
                        let
                            result =
                                case responseBytesOrError of
                                    Err Web.HttpBadUrl ->
                                        Err "Malformed URL"

                                    Err (Web.HttpNetworkError message) ->
                                        Err message

                                    Err (Web.HttpBadStatus _) ->
                                        Err "Bad response status"

                                    Ok responseBytes ->
                                        case responseBytes |> Bytes.Decode.decode (Bytes.Decode.string (responseBytes |> Bytes.width)) of
                                            Nothing ->
                                                Err "failed to decode HTTP body bytes as UTF-8"

                                            Just responseString ->
                                                case responseString |> Json.Decode.decodeString randomQuoteJsonDecoder of
                                                    Err jsonError ->
                                                        Err (jsonError |> Json.Decode.errorToString)

                                                    Ok quote ->
                                                        Ok quote
                        in
                        State { state | quotes = Just result }
            )


randomQuoteJsonDecoder : Json.Decode.Decoder { content : String, author : String }
randomQuoteJsonDecoder =
    Json.Decode.map2 (\content author -> { content = content, author = author })
        (Json.Decode.field "quote" Json.Decode.string)
        (Json.Decode.field "author" Json.Decode.string)


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
        { quotes : Maybe (Result String { content : String, author : String })
        }


type DiceUiEvent
    = MoreClicked
    | QuotesReceived (Result Web.HttpError Bytes)
