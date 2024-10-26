port module Main exposing (main)

{-| "Flight Booker" from <https://eugenkiss.github.io/7guis/tasks/>.

Do not take inspiration from the ui itself (disabled elements etc),
only from the architecture itself.

-}

import Calendar
import Color
import Json.Decode
import Json.Encode
import Random.Pcg.Extended
import Time
import Web


main : Web.Program State
main =
    Web.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


interface : State -> Web.Interface State
interface state =
    case state of
        Submitted submitted ->
            frameUi []
                [ Web.domText
                    (case submitted of
                        SubmittedOneWayFlight oneWayFlight ->
                            "You have booked a one-way flight on "
                                ++ formatDate oneWayFlight.date
                                ++ "."

                        SubmittedReturnFlight returnFlight ->
                            "You have booked a return flight departing on "
                                ++ formatDate returnFlight.departureDate
                                ++ " and "
                                ++ " returning on "
                                ++ formatDate returnFlight.returnDate
                                ++ "."
                    )
                ]
                |> Web.domRender

        NotSubmitted notSubmitted ->
            notSubmittedInterface notSubmitted


type FlightType
    = OneWayFlight
    | ReturnFlight


type State
    = NotSubmitted StateNotSubmitted
    | Submitted SubmittedFlight


type SubmittedFlight
    = SubmittedOneWayFlight
        { date : Calendar.Date
        }
    | SubmittedReturnFlight
        { departureDate : Calendar.Date
        , returnDate : Calendar.Date
        }


type alias StateNotSubmitted =
    { flightType : FlightType
    , departureDateInput : String
    , returnDateInput : String
    }


initialState : State
initialState =
    NotSubmitted
        { flightType = OneWayFlight
        , departureDateInput = formatDate dummyDate
        , returnDateInput = formatDate dummyDate
        }


dummyDate : Calendar.Date
dummyDate =
    { year = 2022
    , month = Time.Jun
    , day = 14
    }
        |> Calendar.fromRawParts
        |> Maybe.withDefault (Calendar.fromPosix (Time.millisToPosix 0))


frameUi : List (Web.DomModifier future) -> List (Web.DomNode future) -> Web.DomNode future
frameUi additionalModifiers subs =
    Web.domElement "div"
        ([ Web.domStyle "display" "flex"
         , Web.domStyle "flex-direction" "column"
         , Web.domStyle "gap" "24px"
         , Web.domStyle "padding" "40px"
         ]
            ++ additionalModifiers
        )
        subs


notSubmittedInterface : StateNotSubmitted -> Web.Interface State
notSubmittedInterface state =
    let
        departureDate : Maybe Calendar.Date
        departureDate =
            inputStringToDate state.departureDateInput

        returnDate : Maybe Calendar.Date
        returnDate =
            inputStringToDate state.returnDateInput
    in
    frameUi
        [ Web.domStyle "width" "230px"
        ]
        [ Web.domElement "select"
            [ domInputStyleBase
            , domInputChangeListen
                |> Web.domModifierFutureMap
                    (\asString ->
                        NotSubmitted
                            { state
                                | flightType =
                                    case asString of
                                        "Return flight" ->
                                            ReturnFlight

                                        _ ->
                                            OneWayFlight
                            }
                    )
            ]
            [ Web.domElement "option" [] [ Web.domText (flightTypeToString OneWayFlight) ]
            , Web.domElement "option" [] [ Web.domText (flightTypeToString ReturnFlight) ]
            ]
        , dateInputUi
            { id = "departure"
            , label = "Departure date (DD.MM.YYYY)"
            , value = state.departureDateInput
            , date = departureDate
            , isDisabled = False
            }
            |> Web.domFutureMap
                (\newInput -> NotSubmitted { state | departureDateInput = newInput })
        , dateInputUi
            { id = "return"
            , label = "Return date (DD.MM.YYYY)"
            , value = state.returnDateInput
            , date = returnDate
            , isDisabled = state.flightType == OneWayFlight
            }
            |> Web.domFutureMap
                (\newInput -> NotSubmitted { state | returnDateInput = newInput })
        , Web.domElement "button"
            [ domInputStyleBase
            , Web.domStyle "padding" "12px 24px"
            , case { flightType = state.flightType, departureDate = departureDate, returnDate = returnDate } |> toSubmitted of
                Nothing ->
                    Web.domBoolProperty "disabled" True

                Just submitted ->
                    Web.domListenTo "pointerdown"
                        |> Web.domModifierFutureMap
                            (\_ -> Submitted submitted)
            ]
            [ Web.domText "Book" ]
        ]
        |> Web.domRender


domInputStyleBase : Web.DomModifier future_
domInputStyleBase =
    [ Web.domStyle "box-sizing" "border-box"
    , Web.domStyle "width" "100%"
    , Web.domStyle "padding" "8px 16px"
    , Web.domStyle "border" "1px solid black"
    ]
        |> Web.domModifierBatch


flightTypeToString : FlightType -> String
flightTypeToString flightType =
    case flightType of
        OneWayFlight ->
            "One-way flight"

        ReturnFlight ->
            "Return flight"


dateInputUi :
    { id : String
    , label : String
    , value : String
    , date : Maybe Calendar.Date
    , isDisabled : Bool
    }
    -> Web.DomNode String
dateInputUi dateInputConfig =
    Web.domElement "div"
        []
        [ Web.domElement "label"
            [ Web.domAttribute "for" dateInputConfig.id
            , Web.domStyle "margin-bottom" "4px"
            ]
            [ Web.domText dateInputConfig.label ]
        , Web.domElement "input"
            [ Web.domStringProperty "value" dateInputConfig.value
            , Web.domAttribute "type" "text"
            , domInputStyleBase
            , domInputChangeListen
            , Web.domBoolProperty "disabled" dateInputConfig.isDisabled
            , Web.domStyle "background-color"
                (case dateInputConfig.date of
                    Nothing ->
                        "red"

                    Just _ ->
                        "white"
                )
            ]
            []
        ]


formatDate : Calendar.Date -> String
formatDate date =
    (date |> Calendar.getDay |> String.fromInt)
        ++ "."
        ++ (date |> Calendar.getMonth |> Calendar.monthToInt |> String.fromInt)
        ++ "."
        ++ (date |> Calendar.getYear |> String.fromInt)


toSubmitted :
    { flightType : FlightType
    , departureDate : Maybe Calendar.Date
    , returnDate : Maybe Calendar.Date
    }
    -> Maybe SubmittedFlight
toSubmitted state =
    case state.departureDate of
        Nothing ->
            Nothing

        Just departureDate ->
            case state.flightType of
                OneWayFlight ->
                    Just (SubmittedOneWayFlight { date = departureDate })

                ReturnFlight ->
                    case state.returnDate of
                        Nothing ->
                            Nothing

                        Just returnDate ->
                            case Calendar.compare departureDate returnDate of
                                LT ->
                                    Just (SubmittedReturnFlight { departureDate = departureDate, returnDate = returnDate })

                                EQ ->
                                    Just (SubmittedReturnFlight { departureDate = departureDate, returnDate = returnDate })

                                GT ->
                                    Nothing


domInputChangeListen : Web.DomModifier String
domInputChangeListen =
    Web.domListenTo "input"
        |> Web.domModifierFutureMap
            (\eventJson ->
                case eventJson |> Json.Decode.decodeValue (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string)) of
                    Ok newValue ->
                        newValue

                    Err error ->
                        -- silent error
                        ""
            )


inputStringToDate : String -> Maybe Calendar.Date
inputStringToDate dateString =
    case dateString |> String.split "." |> List.map String.toInt of
        [ Just day, Just monthInt, Just year ] ->
            case monthInt |> intToMonth of
                Nothing ->
                    Nothing

                Just month ->
                    { day = day, month = month, year = year }
                        |> Calendar.fromRawParts

        _ ->
            Nothing


intToMonth : Int -> Maybe Time.Month
intToMonth monthInt =
    case monthInt of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
