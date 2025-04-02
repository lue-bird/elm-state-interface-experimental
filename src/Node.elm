module Node exposing
    ( program, Program
    , Interface, interfaceBatch, interfaceNone, interfaceFutureMap
    , timePosixRequest, timeZoneRequest, timeZoneNameRequest
    , timePeriodicallyListen, timeOnceAt
    , workingDirectoryPathRequest, launchArgumentsRequest, processTitleSet, exit
    , standardOutWrite, standardErrWrite, standardInListen, standardInRawListen, StreamReadEvent(..)
    , terminalSizeRequest, terminalSizeChangeListen
    , FileKind(..), fileInfoRequest, directorySubPathsRequest
    , directoryMake, fileWrite, fileRequest, fileRemove
    , fileChangeListen, FileChange(..)
    , httpRequestSend, HttpError(..)
    , httpRequestListenAndMaybeRespond, HttpServerEvent(..)
    , randomUnsignedInt32sRequest
    , ProgramConfig, programInit, programUpdate, programSubscriptions
    , ProgramState(..), ProgramEvent(..), InterfaceSingle(..)
    )

{-| A state-interface program that can run in [node.js](https://nodejs.org/en)
which is commonly used to create command line tools, scripts and servers.

I recommend starting with [the minimal setup](https://github.com/lue-bird/elm-state-interface-node-hello),
browsing [the examples](https://github.com/lue-bird/elm-state-interface-experimental/tree/main/example)
and picking parts you want to experiment with.

@docs program, Program

You can also [embed](#embed) a state-interface program as part of an existing app that uses The Elm Architecture.


# interface

@docs Interface, interfaceBatch, interfaceNone, interfaceFutureMap


## time

See [`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/)

@docs timePosixRequest, timeZoneRequest, timeZoneNameRequest
@docs timePeriodicallyListen, timeOnceAt


## process and terminal

@docs workingDirectoryPathRequest, launchArgumentsRequest, processTitleSet, exit
@docs standardOutWrite, standardErrWrite, standardInListen, standardInRawListen, StreamReadEvent
@docs terminalSizeRequest, terminalSizeChangeListen


## file system

@docs FileKind, fileInfoRequest, directorySubPathsRequest
@docs directoryMake, fileWrite, fileRequest, fileRemove
@docs fileChangeListen, FileChange


## HTTP


### client

@docs httpRequestSend, HttpError


### server

@docs httpRequestListenAndMaybeRespond, HttpServerEvent


## random

Not familiar with random "generators"? [`elm/random`](https://package.elm-lang.org/packages/elm/random/latest)
explains it nicely!

@docs randomUnsignedInt32sRequest


## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs ProgramConfig, programInit, programUpdate, programSubscriptions

Under the hood, [`Node.program`](Node#program) is then defined as just

    program config =
        Platform.worker
            { init = \() -> Node.programInit yourAppConfig
            , update = Node.programUpdate yourAppConfig
            , subscriptions = Node.programSubscriptions yourAppConfig
            }


## internals, safe to ignore for users

Exposed so can for example simulate it more easily in tests, add a debugger etc.

@docs ProgramState, ProgramEvent, InterfaceSingle

If you need more things like json encoders/decoders, [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)

-}

import AsciiString
import Bytes exposing (Bytes)
import Duration exposing (Duration)
import FastDict
import Json.Decode
import Json.Decode.LocalExtra
import Json.Encode
import Json.Encode.LocalExtra
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Result.LocalExtra
import StructuredId exposing (StructuredId)
import Time
import Time.LocalExtra



{- Hey! Here's how to add a new interface:


   - choose a name with roughly the shape `DomainSubjectVerb`

   - to `Node.InterfaceSingle`, add a variant `| [YourName] ..additional info (and future handles)..`

   - inside `Node.interfaceSingleFutureJsonDecoder`, specify what js values you expect to decode

   - inside `Node.interfaceSingleToStructuredId`, assign a unique identifier to your interface

     This helps recognize when interfaces have been added, have changed or have been deleted.
     It usually contains all the info from the `DomainSubjectVerb` variant (not including functions)

   - in `runner/node.ts` inside `interfaceAddImplementation`, add
     ```javascript
     case "[YourName]": return (yourInput) => {
         // perform your stuff

         // in case you want to send something to elm, use
         sendToElm({- your value -})

         // in case you want to do something once the interface gets removed,
         // either use the `abortSignal` directly or add
         abortSignal.addEventListener("abort", _event => {
             // remove your stuff
         })
     }
     ```

-}


{-| The "model" in a [`Node.program`](#program)
-}
type ProgramState appState
    = State
        { interface : Interface appState
        , appState : appState
        }


{-| What's needed to create a state-interface [`program`](#program)
-}
type alias ProgramConfig state =
    RecordWithoutConstructorFunction
        { initialState : state
        , interface : state -> Interface state
        , ports :
            { toJs : Json.Encode.Value -> Cmd (ProgramEvent state)
            , fromJs : (Json.Encode.Value -> ProgramEvent state) -> Sub (ProgramEvent state)
            }
        }


{-| Incoming and outgoing effects.
To create one, use the helpers in [time](#time), [HTTP](#http) etc.

To combine multiple, use [`Node.interfaceBatch`](#interfaceBatch) and [`Node.interfaceNone`](#interfaceNone).
To change the value that comes back in the future, use [`Node.interfaceFutureMap`](Node#interfaceFutureMap)

-}
type alias Interface future =
    FastDict.Dict String (InterfaceSingle future)


{-| A "non-batched" [`Interface`](#Interface).
To create one, use the helpers in [time](#time), [HTTP](#http) etc.
-}
type InterfaceSingle future
    = WorkingDirectoryPathRequest (String -> future)
    | LaunchArgumentsRequest (List String -> future)
    | ProcessTitleSet String
    | StandardOutWrite String
    | StandardErrWrite String
    | StandardInListen (String -> future)
    | StandardInRawListen (StreamReadEvent -> future)
    | TerminalSizeRequest ({ lines : Int, columns : Int } -> future)
    | TerminalSizeChangeListen ({ lines : Int, columns : Int } -> future)
    | HttpRequestSend
        { url : String
        , method : String
        , headers : List { name : String, value : String }
        , bodyAsciiString : Maybe String
        , on : Result HttpError Bytes -> future
        }
    | HttpRequestListen
        { portNumber : Int
        , on : HttpServerEvent -> future
        }
    | HttpResponseSend
        { portNumber : Int
        , statusCode : Int
        , headers : List { name : String, value : String }
        , dataAsciiString : String
        }
    | TimePosixRequest (Time.Posix -> future)
    | TimezoneOffsetRequest (Int -> future)
    | TimeOnce { pointInTime : Time.Posix, on : Time.Posix -> future }
    | TimePeriodicallyListen { intervalDurationMilliSeconds : Int, on : Time.Posix -> future }
    | TimezoneNameRequest (String -> future)
    | RandomUnsignedInt32sRequest { count : Int, on : List Int -> future }
    | Exit Int
    | DirectoryMake
        { path : String
        , on : Result { code : String, message : String } () -> future
        }
    | FileRemove String
    | FileWrite
        { contentAsciiString : String
        , path : String
        , on : Result { code : String, message : String } () -> future
        }
    | FileRequest
        { path : String
        , on : Result { code : String, message : String } Bytes -> future
        }
    | FileChangeListen { path : String, on : FileChange -> future }
    | FileInfoRequest
        { path : String
        , on :
            Maybe { byteCount : Int, lastContentChangeTime : Time.Posix, kind : FileKind }
            -> future
        }
    | DirectorySubPathsRequest
        { path : String
        , on : Result { code : String, message : String } (List String) -> future
        }


{-| When data is sent (possibly in chunks), you'll get a `StreamDataReceived` event.
Once it finished sending data, you'll get a `StreamDataEndReached` event

Used in [`Node.standardInListen`](#standardInListen)

Uses the readable stream [data event](https://nodejs.org/api/stream.html#event-data)
and [end event](https://nodejs.org/api/stream.html#event-end)

-}
type StreamReadEvent
    = StreamDataReceived String
    | StreamDataEndReached


{-| How the connection has changed or what request has been sent.
-}
type HttpServerEvent
    = HttpServerOpened
    | HttpRequestReceived
        { method : String
        , headers : List { name : String, value : String }
        , data : Bytes
        }
    | HttpResponseSent
    | HttpServerFailed { code : String, message : String }


{-| Does the path point to a directory or "content"-file
-}
type FileKind
    = KindDirectory
    | KindFile


{-| Did the file at a path get changed/created or moved away/removed?
-}
type FileChange
    = FileAddedOrChanged String
    | FileRemoved String


{-| Combine multiple [`Interface`](#Interface)s into one
-}
interfaceBatch : List (Interface future) -> Interface future
interfaceBatch interfaces =
    interfaces |> List.foldl FastDict.union FastDict.empty


{-| Doing nothing as an [`Interface`](#Interface). These two examples are equivalent:

    Node.interfaceBatch [ a, Node.interfaceNone, b ]

and

    Node.interfaceBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
interfaceNone : Interface future_
interfaceNone =
    FastDict.empty


{-| Take what the [`Interface`](#Interface) can come back with and return a different future value.

In practice, this is sometimes used like a kind of event-config pattern:

    Node.timePosixRequest
        |> Node.interfaceFutureMap (\timeNow -> TimeReceived timeNow)

sometimes as a way to deal with all events (like `update` in The Elm Architecture)

    ...
        |> Node.interfaceFutureMap
            (\event ->
                case event of
                    CursorMovedTo newCursorPoint ->
                        { state | cursorPoint = newCursorPoint }

                    MinusEntered ->
                        { state | counter = state.counter - 1 }

                    PlusEntered ->
                        { state | counter = state.counter + 1 }
            )

In all these examples, you end up converting the narrow future representation of part of the interface
to a broader representation for the parent interface

-}
interfaceFutureMap : (future -> mappedFuture) -> Interface future -> Interface mappedFuture
interfaceFutureMap futureChange interface =
    interface
        |> FastDict.map
            (\_ interfaceSingle ->
                interfaceSingle |> interfaceSingleFutureMap futureChange
            )


interfaceSingleFutureMap : (future -> mappedFuture) -> InterfaceSingle future -> InterfaceSingle mappedFuture
interfaceSingleFutureMap futureChange interfaceSingle =
    case interfaceSingle of
        HttpRequestSend send ->
            { url = send.url
            , method = send.method
            , headers = send.headers
            , bodyAsciiString = send.bodyAsciiString
            , on = \responseBytes -> send.on responseBytes |> futureChange
            }
                |> HttpRequestSend

        HttpRequestListen listen ->
            HttpRequestListen
                { portNumber = listen.portNumber
                , on = \request -> listen.on request |> futureChange
                }

        HttpResponseSend send ->
            HttpResponseSend send

        TimePosixRequest requestTimeNow ->
            (\event -> requestTimeNow event |> futureChange)
                |> TimePosixRequest

        TimezoneOffsetRequest requestTimezone ->
            (\event -> requestTimezone event |> futureChange)
                |> TimezoneOffsetRequest

        TimezoneNameRequest requestTimezoneName ->
            (\event -> requestTimezoneName event |> futureChange)
                |> TimezoneNameRequest

        TimeOnce once ->
            { pointInTime = once.pointInTime
            , on = \event -> event |> once.on |> futureChange
            }
                |> TimeOnce

        RandomUnsignedInt32sRequest request ->
            { count = request.count
            , on = \ints -> request.on ints |> futureChange
            }
                |> RandomUnsignedInt32sRequest

        TimePeriodicallyListen periodicallyListen ->
            { intervalDurationMilliSeconds = periodicallyListen.intervalDurationMilliSeconds
            , on = \posix -> periodicallyListen.on posix |> futureChange
            }
                |> TimePeriodicallyListen

        Exit code ->
            Exit code

        DirectoryMake make ->
            DirectoryMake
                { path = make.path
                , on = \result -> make.on result |> futureChange
                }

        FileRemove path ->
            FileRemove path

        FileWrite write ->
            FileWrite
                { path = write.path
                , contentAsciiString = write.contentAsciiString
                , on = \result -> write.on result |> futureChange
                }

        FileRequest request ->
            FileRequest
                { path = request.path
                , on = \content -> request.on content |> futureChange
                }

        FileChangeListen listen ->
            FileChangeListen
                { path = listen.path
                , on = \fileChange -> listen.on fileChange |> futureChange
                }

        FileInfoRequest request ->
            FileInfoRequest
                { path = request.path
                , on = \info -> request.on info |> futureChange
                }

        DirectorySubPathsRequest request ->
            DirectorySubPathsRequest
                { path = request.path
                , on = \subNames -> request.on subNames |> futureChange
                }

        WorkingDirectoryPathRequest on ->
            WorkingDirectoryPathRequest (\path -> on path |> futureChange)

        LaunchArgumentsRequest on ->
            LaunchArgumentsRequest (\arguments -> on arguments |> futureChange)

        TerminalSizeRequest on ->
            TerminalSizeRequest (\size -> on size |> futureChange)

        TerminalSizeChangeListen on ->
            TerminalSizeChangeListen (\size -> on size |> futureChange)

        ProcessTitleSet newTitle ->
            ProcessTitleSet newTitle

        StandardOutWrite text ->
            StandardOutWrite text

        StandardErrWrite text ->
            StandardErrWrite text

        StandardInListen on ->
            StandardInListen (\size -> on size |> futureChange)

        StandardInRawListen on ->
            StandardInRawListen (\size -> on size |> futureChange)


{-| The "msg" in a [`Node.program`](#program)
-}
type ProgramEvent appState
    = JsEventFailedToDecode Json.Decode.Error
    | JsEventEnabledConstructionOfNewAppState appState


idAndDiffToJson : String -> InterfaceSingleDiff future_ -> Json.Encode.Value
idAndDiffToJson id diff =
    Json.Encode.object
        [ ( "id", id |> Json.Encode.string )
        , ( "diff", diff |> interfaceSingleDiffToJson )
        ]


interfaceSingleDiffToJson : InterfaceSingleDiff future_ -> Json.Encode.Value
interfaceSingleDiffToJson diff =
    Json.Encode.LocalExtra.variant
        (case diff of
            Add interfaceSingleInfo ->
                { tag = "Add", value = interfaceSingleInfo |> interfaceSingleToJson }

            Remove () ->
                { tag = "Remove", value = Json.Encode.null }
        )


interfaceSingleToJson : InterfaceSingle future_ -> Json.Encode.Value
interfaceSingleToJson interfaceSingle =
    Json.Encode.LocalExtra.variant
        (case interfaceSingle of
            HttpRequestSend send ->
                { tag = "HttpRequestSend"
                , value =
                    Json.Encode.object
                        [ ( "url", send.url |> Json.Encode.string )
                        , ( "method", send.method |> Json.Encode.string )
                        , ( "headers"
                          , send.headers
                                |> Json.Encode.list
                                    (\header ->
                                        Json.Encode.object
                                            [ ( "name", header.name |> Json.Encode.string )
                                            , ( "value", header.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        , ( "bodyAsciiString"
                          , send.bodyAsciiString
                                |> Json.Encode.LocalExtra.nullable Json.Encode.string
                          )
                        ]
                }

            HttpRequestListen listen ->
                { tag = "HttpRequestListen"
                , value = Json.Encode.object [ ( "port", listen.portNumber |> Json.Encode.int ) ]
                }

            HttpResponseSend send ->
                { tag = "HttpResponseSend"
                , value =
                    Json.Encode.object
                        [ ( "port", send.portNumber |> Json.Encode.int )
                        , ( "statusCode", send.statusCode |> Json.Encode.int )
                        , ( "headers"
                          , send.headers
                                |> Json.Encode.list
                                    (\header ->
                                        Json.Encode.object
                                            [ ( "name", header.name |> Json.Encode.string )
                                            , ( "value", header.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        , ( "dataAsciiString", send.dataAsciiString |> Json.Encode.string )
                        ]
                }

            TimePosixRequest _ ->
                { tag = "TimePosixRequest", value = Json.Encode.null }

            TimezoneOffsetRequest _ ->
                { tag = "TimezoneOffsetRequest", value = Json.Encode.null }

            TimezoneNameRequest _ ->
                { tag = "TimezoneNameRequest", value = Json.Encode.null }

            TimeOnce once ->
                { tag = "TimeOnce"
                , value =
                    Json.Encode.object
                        [ ( "pointInTime", once.pointInTime |> Time.posixToMillis |> Json.Encode.int ) ]
                }

            RandomUnsignedInt32sRequest request ->
                { tag = "RandomUnsignedInt32sRequest", value = request.count |> Json.Encode.int }

            TimePeriodicallyListen intervalDuration ->
                { tag = "TimePeriodicallyListen"
                , value =
                    Json.Encode.object
                        [ ( "milliSeconds", intervalDuration.intervalDurationMilliSeconds |> Json.Encode.int ) ]
                }

            Exit code ->
                { tag = "Exit", value = code |> Json.Encode.int }

            DirectoryMake make ->
                { tag = "DirectoryMake", value = make.path |> Json.Encode.string }

            FileRemove path ->
                { tag = "FileRemove", value = path |> Json.Encode.string }

            FileWrite write ->
                { tag = "FileWrite"
                , value =
                    Json.Encode.object
                        [ ( "path", write.path |> Json.Encode.string )
                        , ( "contentAsciiString", write.contentAsciiString |> Json.Encode.string )
                        ]
                }

            FileRequest request ->
                { tag = "FileRequest"
                , value = request.path |> Json.Encode.string
                }

            FileChangeListen listen ->
                { tag = "FileChangeListen"
                , value = listen.path |> Json.Encode.string
                }

            FileInfoRequest request ->
                { tag = "FileInfoRequest"
                , value = request.path |> Json.Encode.string
                }

            DirectorySubPathsRequest request ->
                { tag = "DirectorySubPathsRequest"
                , value = request.path |> Json.Encode.string
                }

            WorkingDirectoryPathRequest _ ->
                { tag = "WorkingDirectoryPathRequest", value = Json.Encode.null }

            LaunchArgumentsRequest _ ->
                { tag = "LaunchArgumentsRequest", value = Json.Encode.null }

            TerminalSizeRequest _ ->
                { tag = "TerminalSizeRequest", value = Json.Encode.null }

            TerminalSizeChangeListen _ ->
                { tag = "TerminalSizeChangeListen", value = Json.Encode.null }

            ProcessTitleSet newTitle ->
                { tag = "ProcessTitleSet", value = newTitle |> Json.Encode.string }

            StandardOutWrite text ->
                { tag = "StandardOutWrite", value = text |> Json.Encode.string }

            StandardErrWrite text ->
                { tag = "StandardErrWrite", value = text |> Json.Encode.string }

            StandardInListen _ ->
                { tag = "StandardInListen", value = Json.Encode.null }

            StandardInRawListen _ ->
                { tag = "StandardInRawListen", value = Json.Encode.null }
        )


interfaceSingleToStructuredId : InterfaceSingle future_ -> StructuredId
interfaceSingleToStructuredId interfaceSingle =
    StructuredId.ofVariant
        (case interfaceSingle of
            HttpRequestSend request ->
                { tag = "HttpRequestSend"
                , value = request.url |> StructuredId.ofString
                }

            HttpRequestListen listen ->
                { tag = "HttpRequestListen"
                , value = listen.portNumber |> StructuredId.ofInt
                }

            HttpResponseSend send ->
                { tag = "HttpResponseSend"
                , value =
                    Json.Encode.object
                        [ ( "port", send.portNumber |> StructuredId.ofInt )
                        , ( "statusCode", send.statusCode |> StructuredId.ofInt )
                        , ( "headers"
                          , send.headers
                                |> StructuredId.ofList
                                    (\header ->
                                        Json.Encode.object
                                            [ ( "name", header.name |> StructuredId.ofString )
                                            , ( "value", header.value |> StructuredId.ofString )
                                            ]
                                    )
                          )
                        , ( "dataAsciiString", send.dataAsciiString |> StructuredId.ofString )
                        ]
                }

            TimePosixRequest _ ->
                { tag = "TimePosixRequest", value = StructuredId.ofUnit }

            TimezoneOffsetRequest _ ->
                { tag = "TimezoneOffsetRequest", value = StructuredId.ofUnit }

            TimezoneNameRequest _ ->
                { tag = "TimezoneNameRequest", value = StructuredId.ofUnit }

            TimeOnce once ->
                { tag = "TimeOnce"
                , value = once.pointInTime |> Time.LocalExtra.posixToStructureId
                }

            RandomUnsignedInt32sRequest request ->
                { tag = "RandomUnsignedInt32sRequest"
                , value = request.count |> StructuredId.ofInt
                }

            TimePeriodicallyListen listen ->
                { tag = "TimePeriodicallyListen"
                , value = listen.intervalDurationMilliSeconds |> StructuredId.ofInt
                }

            Exit _ ->
                { tag = "Exit", value = StructuredId.ofUnit }

            DirectoryMake make ->
                { tag = "DirectoryMake"
                , value = make.path |> StructuredId.ofString
                }

            FileRemove path ->
                { tag = "FileRemove"
                , value = path |> StructuredId.ofString
                }

            FileWrite write ->
                { tag = "FileWrite"
                , value =
                    StructuredId.ofParts
                        [ write.path |> StructuredId.ofString
                        , write.contentAsciiString |> StructuredId.ofString
                        ]
                }

            FileRequest request ->
                { tag = "FileRequest"
                , value = request.path |> StructuredId.ofString
                }

            FileChangeListen listen ->
                { tag = "FileChangeListen"
                , value = listen.path |> StructuredId.ofString
                }

            FileInfoRequest request ->
                { tag = "FileInfoRequest"
                , value = request.path |> StructuredId.ofString
                }

            DirectorySubPathsRequest request ->
                { tag = "DirectorySubPathsRequest"
                , value = request.path |> StructuredId.ofString
                }

            WorkingDirectoryPathRequest _ ->
                { tag = "WorkingDirectoryPathRequest", value = StructuredId.ofUnit }

            LaunchArgumentsRequest _ ->
                { tag = "LaunchArgumentsRequest", value = StructuredId.ofUnit }

            TerminalSizeRequest _ ->
                { tag = "TerminalSizeRequest", value = StructuredId.ofUnit }

            TerminalSizeChangeListen _ ->
                { tag = "TerminalSizeChangeListen", value = StructuredId.ofUnit }

            ProcessTitleSet title ->
                { tag = "ProcessTitleSet"
                , value = title |> StructuredId.ofString
                }

            StandardOutWrite text ->
                { tag = "StandardOutWrite"
                , value = text |> StructuredId.ofString
                }

            StandardErrWrite text ->
                { tag = "StandardErrWrite"
                , value = text |> StructuredId.ofString
                }

            StandardInListen _ ->
                { tag = "StandardInListen", value = StructuredId.ofUnit }

            StandardInRawListen _ ->
                { tag = "StandardInRawListen", value = StructuredId.ofUnit }
        )


{-| The "init" part for an embedded program
-}
programInit : ProgramConfig state -> ( ProgramState state, Cmd (ProgramEvent state) )
programInit appConfig =
    let
        initialInterface : Interface state
        initialInterface =
            appConfig.initialState
                |> appConfig.interface
    in
    ( State
        { interface = initialInterface
        , appState = appConfig.initialState
        }
    , initialInterface
        |> FastDict.foldl
            (\id new soFar ->
                appConfig.ports.toJs (idAndDiffToJson id (new |> Add))
                    :: soFar
            )
            []
        |> Cmd.batch
    )


{-| The "subscriptions" part for an embedded program
-}
programSubscriptions : ProgramConfig state -> ProgramState state -> Sub (ProgramEvent state)
programSubscriptions appConfig (State state) =
    appConfig.ports.fromJs
        (\interfaceJson ->
            interfaceJson
                |> Json.Decode.decodeValue
                    (idStringJsonDecoder
                        |> Json.Decode.andThen
                            (\originalInterfaceId ->
                                case state.interface |> FastDict.get originalInterfaceId of
                                    Just interfaceSingleAcceptingFuture ->
                                        case interfaceSingleAcceptingFuture |> interfaceSingleFutureJsonDecoder of
                                            Just eventDataDecoder ->
                                                Json.Decode.field "eventData" eventDataDecoder

                                            Nothing ->
                                                "interface did not expect any events" |> Json.Decode.fail

                                    Nothing ->
                                        "no associated interface found among ids\n"
                                            ++ (state.interface
                                                    |> FastDict.keys
                                                    |> String.join "\n"
                                               )
                                            |> Json.Decode.fail
                            )
                        |> Json.Decode.map JsEventEnabledConstructionOfNewAppState
                    )
                |> Result.LocalExtra.valueOrOnError JsEventFailedToDecode
        )


idStringJsonDecoder : Json.Decode.Decoder String
idStringJsonDecoder =
    Json.Decode.field "id" Json.Decode.string


{-| [json `Decoder`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Decoder)
for the transformed event data coming back
-}
interfaceSingleFutureJsonDecoder : InterfaceSingle future -> Maybe (Json.Decode.Decoder future)
interfaceSingleFutureJsonDecoder interface =
    case interface of
        HttpRequestSend send ->
            Json.Decode.LocalExtra.resultOkErr
                httpSuccessResponseJsonDecoder
                (httpErrorJsonDecoder
                    |> Json.Decode.map Err
                )
                |> Json.Decode.map send.on
                |> Just

        HttpRequestListen listen ->
            httpServerEventJsonDecoder
                |> Json.Decode.map listen.on
                |> Just

        HttpResponseSend _ ->
            Nothing

        TimePosixRequest toFuture ->
            Time.LocalExtra.posixJsonDecoder |> Json.Decode.map toFuture |> Just

        TimezoneOffsetRequest toFuture ->
            Json.Decode.int |> Json.Decode.map toFuture |> Just

        TimePeriodicallyListen periodicallyListen ->
            Time.LocalExtra.posixJsonDecoder
                |> Json.Decode.map periodicallyListen.on
                |> Just

        TimeOnce once ->
            Time.LocalExtra.posixJsonDecoder
                |> Json.Decode.map once.on
                |> Just

        TimezoneNameRequest toFuture ->
            Json.Decode.string |> Json.Decode.map toFuture |> Just

        RandomUnsignedInt32sRequest request ->
            Json.Decode.list Json.Decode.int
                |> Json.Decode.map request.on
                |> Just

        Exit _ ->
            Nothing

        DirectoryMake make ->
            directoryMakeResultJsonDecoder
                |> Json.Decode.map make.on
                |> Just

        FileRemove _ ->
            Nothing

        FileWrite write ->
            fileWriteResultJsonDecoder
                |> Json.Decode.map write.on
                |> Just

        FileRequest request ->
            fileResultJsonDecoder
                |> Json.Decode.map request.on
                |> Just

        FileChangeListen listen ->
            fileChangeJsonDecoder
                |> Json.Decode.map listen.on
                |> Just

        FileInfoRequest request ->
            fileInfoJsonDecoder
                |> Json.Decode.map request.on
                |> Just

        DirectorySubPathsRequest request ->
            directorySubPathsResultJsonDecoder
                |> Json.Decode.map request.on
                |> Just

        WorkingDirectoryPathRequest on ->
            Json.Decode.string
                |> Json.Decode.map on
                |> Just

        LaunchArgumentsRequest on ->
            launchArgumentsJsonDecoder
                |> Json.Decode.map on
                |> Just

        TerminalSizeRequest on ->
            terminalSizeJsonDecoder
                |> Json.Decode.map on
                |> Just

        TerminalSizeChangeListen on ->
            terminalSizeJsonDecoder
                |> Json.Decode.map on
                |> Just

        ProcessTitleSet _ ->
            Nothing

        StandardOutWrite _ ->
            Nothing

        StandardErrWrite _ ->
            Nothing

        StandardInListen on ->
            Json.Decode.string
                |> Json.Decode.map on
                |> Just

        StandardInRawListen on ->
            streamReadEventJsonDecoder
                |> Json.Decode.map on
                |> Just


streamReadEventJsonDecoder : Json.Decode.Decoder StreamReadEvent
streamReadEventJsonDecoder =
    Json.Decode.LocalExtra.choice
        [ { tag = "StreamDataReceived"
          , value =
                Json.Decode.map StreamDataReceived
                    Json.Decode.string
          }
        , { tag = "StreamDataEndReached"
          , value =
                Json.Decode.null StreamDataEndReached
          }
        ]


launchArgumentsJsonDecoder : Json.Decode.Decoder (List String)
launchArgumentsJsonDecoder =
    Json.Decode.list Json.Decode.string


fileInfoJsonDecoder : Json.Decode.Decoder (Maybe { kind : FileKind, byteCount : Int, lastContentChangeTime : Time.Posix })
fileInfoJsonDecoder =
    Json.Decode.nullable
        (Json.Decode.map3
            (\kind byteCount lastContentChangeTime ->
                { kind = kind, byteCount = byteCount, lastContentChangeTime = lastContentChangeTime }
            )
            (Json.Decode.field "kind" fileKindJsonDecoder)
            (Json.Decode.field "byteCount" Json.Decode.int)
            (Json.Decode.field "lastContentChangePosixMilliseconds"
                (Json.Decode.map Time.millisToPosix Json.Decode.int)
            )
        )


fileWriteResultJsonDecoder : Json.Decode.Decoder (Result { code : String, message : String } ())
fileWriteResultJsonDecoder =
    Json.Decode.LocalExtra.resultOkErr
        (Json.Decode.null (Ok ()))
        (Json.Decode.map2 (\code message -> Err { code = code, message = message })
            (Json.Decode.oneOf
                [ Json.Decode.field "code" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
            (Json.Decode.oneOf
                [ Json.Decode.field "message" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
        )


directoryMakeResultJsonDecoder : Json.Decode.Decoder (Result { code : String, message : String } ())
directoryMakeResultJsonDecoder =
    Json.Decode.LocalExtra.resultOkErr
        (Json.Decode.null (Ok ()))
        (Json.Decode.map2 (\code message -> Err { code = code, message = message })
            (Json.Decode.oneOf
                [ Json.Decode.field "code" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
            (Json.Decode.oneOf
                [ Json.Decode.field "message" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
        )


fileChangeJsonDecoder : Json.Decode.Decoder FileChange
fileChangeJsonDecoder =
    Json.Decode.LocalExtra.choice
        [ { tag = "Removed"
          , value =
                Json.Decode.map FileRemoved Json.Decode.string
          }
        , { tag = "AddedOrChanged"
          , value =
                Json.Decode.map FileAddedOrChanged Json.Decode.string
          }
        ]


directorySubPathsResultJsonDecoder : Json.Decode.Decoder (Result { code : String, message : String } (List String))
directorySubPathsResultJsonDecoder =
    Json.Decode.LocalExtra.resultOkErr
        (Json.Decode.map Ok
            (Json.Decode.list Json.Decode.string)
        )
        (Json.Decode.map2 (\code message -> Err { code = code, message = message })
            (Json.Decode.oneOf
                [ Json.Decode.field "code" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
            (Json.Decode.oneOf
                [ Json.Decode.field "message" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
        )


fileResultJsonDecoder : Json.Decode.Decoder (Result { code : String, message : String } Bytes)
fileResultJsonDecoder =
    Json.Decode.LocalExtra.resultOkErr
        (Json.Decode.map (\asciiString -> Ok (asciiString |> AsciiString.toBytes))
            Json.Decode.string
        )
        (Json.Decode.map2 (\code message -> Err { code = code, message = message })
            (Json.Decode.oneOf
                [ Json.Decode.field "code" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
            (Json.Decode.oneOf
                [ Json.Decode.field "message" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
        )


httpServerEventJsonDecoder : Json.Decode.Decoder HttpServerEvent
httpServerEventJsonDecoder =
    Json.Decode.LocalExtra.choice
        [ { tag = "HttpServerOpened"
          , value =
                Json.Decode.null HttpServerOpened
          }
        , { tag = "HttpRequestReceived"
          , value =
                Json.Decode.map3
                    (\method headers data ->
                        HttpRequestReceived { method = method, headers = headers, data = data }
                    )
                    (Json.Decode.field "method" Json.Decode.string)
                    (Json.Decode.field "headers"
                        (Json.Decode.list
                            (Json.Decode.map2 (\name value -> { name = name, value = value })
                                (Json.Decode.field "name" Json.Decode.string)
                                (Json.Decode.field "value" Json.Decode.string)
                            )
                        )
                    )
                    (Json.Decode.field "dataAsciiString"
                        (Json.Decode.map AsciiString.toBytes Json.Decode.string)
                    )
          }
        , { tag = "HttpResponseSent"
          , value =
                Json.Decode.null HttpResponseSent
          }
        , { tag = "HttpServerFailed"
          , value =
                Json.Decode.map2
                    (\code message ->
                        HttpServerFailed { code = code, message = message }
                    )
                    (Json.Decode.field "code" Json.Decode.string)
                    (Json.Decode.field "message" Json.Decode.string)
          }
        ]


fileKindJsonDecoder : Json.Decode.Decoder FileKind
fileKindJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> KindFile)
            (Json.Decode.LocalExtra.onlyString "File")
        , Json.Decode.map (\() -> KindDirectory)
            (Json.Decode.LocalExtra.onlyString "Directory")
        ]


terminalSizeJsonDecoder : Json.Decode.Decoder { lines : Int, columns : Int }
terminalSizeJsonDecoder =
    Json.Decode.map2 (\lines columns -> { lines = lines, columns = columns })
        (Json.Decode.field "lines" Json.Decode.int)
        (Json.Decode.field "columns" Json.Decode.int)


httpSuccessResponseJsonDecoder : Json.Decode.Decoder (Result HttpError Bytes)
httpSuccessResponseJsonDecoder =
    httpResponseJsonDecoder
        |> Json.Decode.map
            (\response ->
                if response.statusCode >= 200 && response.statusCode < 300 then
                    Ok response.body

                else
                    Err (HttpBadStatus response)
            )


httpResponseJsonDecoder :
    Json.Decode.Decoder
        { statusCode : Int
        , statusText : String
        , headers : List { name : String, value : String }
        , body : Bytes
        }
httpResponseJsonDecoder =
    Json.Decode.map4
        (\statusCode statusText headers body ->
            { statusCode = statusCode
            , statusText = statusText
            , headers = headers
            , body = body
            }
        )
        (Json.Decode.field "statusCode" Json.Decode.int)
        (Json.Decode.field "statusText" Json.Decode.string)
        (Json.Decode.field "headers"
            (Json.Decode.list
                (Json.Decode.map2
                    (\name value -> { name = name, value = value })
                    (Json.Decode.field "name" Json.Decode.string)
                    (Json.Decode.field "value" Json.Decode.string)
                )
            )
        )
        (Json.Decode.field "bodyAsciiString"
            (Json.Decode.map AsciiString.toBytes Json.Decode.string)
        )


httpErrorJsonDecoder : Json.Decode.Decoder HttpError
httpErrorJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> HttpBadUrl)
            (Json.Decode.field "cause"
                (Json.Decode.field "code" (Json.Decode.LocalExtra.onlyString "BAD_URL"))
            )
        , Json.Decode.map HttpNetworkError
            (Json.Decode.oneOf
                [ Json.Decode.field "message" Json.Decode.string
                , Json.Decode.succeed ""
                ]
            )
        ]


{-| The "update" part for an embedded program
-}
programUpdate : ProgramConfig state -> ProgramEvent state -> ProgramState state -> ( ProgramState state, Cmd (ProgramEvent state) )
programUpdate appConfig event state =
    case event of
        JsEventFailedToDecode jsonError ->
            ( state
            , let
                notifyOfSkippedEventInterface : InterfaceSingle never_
                notifyOfSkippedEventInterface =
                    StandardErrWrite
                        ("js event skipped because: "
                            ++ (jsonError |> Json.Decode.errorToString)
                        )
              in
              idAndDiffToJson
                (notifyOfSkippedEventInterface |> interfaceSingleToStructuredId |> StructuredId.toString)
                (notifyOfSkippedEventInterface |> Add)
                |> appConfig.ports.toJs
            )

        JsEventEnabledConstructionOfNewAppState updatedAppState ->
            let
                (State oldState) =
                    state

                updatedInterface : Interface state
                updatedInterface =
                    updatedAppState |> appConfig.interface
            in
            ( State { interface = updatedInterface, appState = updatedAppState }
            , { old = oldState.interface, updated = updatedInterface }
                |> interfacesDiffMap
                    (\id diff -> appConfig.ports.toJs (idAndDiffToJson id diff))
                |> Cmd.batch
            )


{-| Determine which outgoing effects need to be executed based on the difference between old and updated interfaces
-}
interfacesDiffMap :
    (String -> InterfaceSingleDiff future -> combined)
    ->
        { old : Interface future
        , updated : Interface future
        }
    -> List combined
interfacesDiffMap idAndDiffCombine interfaces =
    FastDict.merge
        (\removedId _ soFar ->
            idAndDiffCombine removedId remove :: soFar
        )
        (\_ _ _ soFar -> soFar)
        (\addedId onlyNew soFar ->
            idAndDiffCombine addedId (onlyNew |> Add)
                :: soFar
        )
        interfaces.old
        interfaces.updated
        []


remove : InterfaceSingleDiff irrelevantFuture_
remove =
    Remove ()


{-| Individual message to js to sync up with the latest interface type
-}
type InterfaceSingleDiff irrelevantFuture
    = Add (InterfaceSingle irrelevantFuture)
    | Remove ()


{-| Create a [`Program`](#Program):

  - The state is everything the program knows (what The Elm Architecture calls model).
    And it always starts with a given `initialState`

  - The [`Interface`](#Interface) is the face to the outside world
    and can be created using the helpers in [time](#time), [HTTP](#http) etc.
    The given `interface` function constructs these interfaces based on the current state

  - Connections to and from js

        port toJs : Json.Encode.Value -> Cmd event_

        port fromJs : (Json.Encode.Value -> event) -> Sub event

-}
program :
    { initialState : state
    , interface : state -> Interface state
    , ports :
        { toJs : Json.Encode.Value -> Cmd (ProgramEvent state)
        , fromJs : (Json.Encode.Value -> ProgramEvent state) -> Sub (ProgramEvent state)
        }
    }
    -> Program state
program appConfig =
    Platform.worker
        { init = \() -> programInit appConfig
        , update = \event state -> programUpdate appConfig event state
        , subscriptions = \state -> programSubscriptions appConfig state
        }


{-| A [`Platform.Program`](https://dark.elm.dmy.fr/packages/elm/core/latest/Platform#Program)
that elm can run,
produced by [`Node.program`](#program)
-}
type alias Program state =
    Platform.Program () (ProgramState state) (ProgramEvent state)


interfaceFromSingle : InterfaceSingle future -> Interface future
interfaceFromSingle interfaceSingle =
    FastDict.singleton
        (interfaceSingle |> interfaceSingleToStructuredId |> StructuredId.toString)
        interfaceSingle


{-| An [`Interface`](Node#Interface) for getting the current [POSIX time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix).

Replacement for [`elm/time`'s `Time.now`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#now).

-}
timePosixRequest : Interface Time.Posix
timePosixRequest =
    TimePosixRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting a [`Time.Zone`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Zone)
based on the current UTC offset.

Replacement for [`elm/time`'s `Time.here`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#here).

-}
timeZoneRequest : Interface Time.Zone
timeZoneRequest =
    TimezoneOffsetRequest (\offset -> Time.customZone -offset [])
        |> interfaceFromSingle


{-| Intended for package authors.
An [`Interface`](Node#Interface) for using [`Intl.DateTimeFormat().resolvedOptions().timeZone`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/resolvedOptions#timezone)
to get names like `Europe/Moscow` or `America/Havana`.
From there you can look it up in any [IANA data](https://www.iana.org/time-zones) you loaded yourself.

Replacement for [`elm/time`'s `Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
timeZoneNameRequest : Interface String
timeZoneNameRequest =
    TimezoneNameRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting a reminder
once a given [point in time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix) has been reached.

This lets you for example wait until it's 15 minutes before the event,
timeout a request or schedule a certain action to a specific time.

    import Node

    type RequestState result
        = NotAsked
        | BeforeTimeout { start : Time.Posix }
        | TimedOut
        | GotResult result

    { initialState = NotAsked
    , interface =
        \state ->
            [ case state of
                NotAsked ->
                    [ Node.timePosixRequest
                        |> Node.interfaceFutureMap BeforeTimeout
                    , ..request.. |> Node.futureMap GotResult
                    ]

                BeforeTimeout requestTime ->
                    -- timeout after 10 seconds
                    Node.timeOnceAt (Duration.addTo requestTime (Duration.seconds 10))
                        |> Node.interfaceFutureMap (\_ -> TimedOut)

                TimedOut ->
                    ...

                GotResult result ->
                    ...
            ]
    }

  - 🧩 [`Duration` is from ianmackenzie/elm-units](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration)

You can abstract this in various ways like adding a

    withTimeout :
        ..Request result..
        -> Node.Interface (RequestState result)

where the result can be put into the "main state" and therefore cased on.

-}
timeOnceAt : Time.Posix -> Interface Time.Posix
timeOnceAt pointInTime =
    TimeOnce { pointInTime = pointInTime, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting the current time
every time a given [`Duration`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration) has passed.
-}
timePeriodicallyListen : Duration -> Interface Time.Posix
timePeriodicallyListen intervalDuration =
    TimePeriodicallyListen
        { intervalDurationMilliSeconds = intervalDuration |> Duration.inMilliseconds |> Basics.round
        , on = identity
        }
        |> interfaceFromSingle


{-| Very gracefully stop the process,
completing all other current interface operations.
Do not keep listens in the interface,
otherwise the process will not die.

Keep in mind that every process ends automatically
once no interfaces that expect future data are left
which means you only need [`Node.exit`](#exit) in case you want to exit
with a non-zero error code.

Uses [`process.exitCode = ...`](https://nodejs.org/api/process.html#processexitcode_1)
instead of [`process.exit`](https://nodejs.org/api/process.html#processexitcode)
to still make interfaces like writing an error come through before exiting.

-}
exit : Int -> Interface future_
exit code =
    Exit code
        |> interfaceFromSingle


{-| Replace the title of the running process.
This will usually display in the title bar of your terminal emulator or in activity monitors.

Uses [`process.title = ...`](https://nodejs.org/api/process.html#processtitle)

-}
processTitleSet : String -> Interface future_
processTitleSet newTitle =
    ProcessTitleSet newTitle
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting the current working directory.

Uses [`process.cwd`](https://nodejs.org/api/process.html#processcwd)

-}
workingDirectoryPathRequest : Interface String
workingDirectoryPathRequest =
    WorkingDirectoryPathRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting
the command-line arguments passed when the Node.js process was launched.

For example, running

    node process-args.js one two=three four

[`Node.launchArgumentsRequest`](#launchArgumentsRequest)
would give back

    [ "/usr/local/bin/node"
    , "/Users/mjr/work/node/process-args.js"
    , "one"
    , "two=three"
    , "four"
    ]

The first is always the absolute path to the executable that started the Node.js process,
the second is always the path to the executed js file.

Uses [`process.argv`](https://nodejs.org/api/process.html#processargv)

-}
launchArgumentsRequest : Interface (List String)
launchArgumentsRequest =
    LaunchArgumentsRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for creating a directory including its parent directories
at a given path unless it already exists.

Uses [`fs.mkdir({ recursive : true })`](https://nodejs.org/api/fs.html#fspromisesmkdirpath-options)

-}
directoryMake : String -> Interface (Result { code : String, message : String } ())
directoryMake path =
    DirectoryMake { path = path, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for unlinking a file
at a given path.

Uses [`fs.unlink`](https://nodejs.org/api/fs.html#fsunlinkpath-callback)

-}
fileRemove : String -> Interface future_
fileRemove path =
    FileRemove path
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for creating or overwriting a file
at a given path with given [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes#Bytes) content.

Uses [`fs.writeFile`](https://nodejs.org/api/fs.html#fswritefilefile-data-options-callback)

-}
fileWrite : { path : String, content : Bytes } -> Interface (Result { code : String, message : String } ())
fileWrite write =
    FileWrite
        { path = write.path
        , contentAsciiString = write.content |> AsciiString.fromBytes
        , on = identity
        }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for reading the [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes#Bytes) content
of the file at a given path.

Uses [`fs.readFile`](https://nodejs.org/api/fs.html#fsreadfilepath-options-callback)

-}
fileRequest : String -> Interface (Result { code : String, message : String } Bytes)
fileRequest path =
    FileRequest { path = path, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for detecting changes to
the file at a given path or any of its deepest sub-files.

Uses [`fs.watch({ recursive: true })`](https://nodejs.org/api/fs.html#fswatchfilename-options-listener)

-}
fileChangeListen : String -> Interface FileChange
fileChangeListen path =
    FileChangeListen { path = path, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting file metadata at a given path
or `Nothing` if no file exists there.

    fileExistsRequest : String -> Node.Interface Bool
    fileExistsRequest path =
        Node.fileInfoRequest path
            |> Node.interfaceFutureRequest (\info -> info /= Nothing)

Uses [`fs.stat`](https://nodejs.org/api/fs.html#fspromisesstatpath-options)

-}
fileInfoRequest :
    String
    -> Interface (Maybe { byteCount : Int, lastContentChangeTime : Time.Posix, kind : FileKind })
fileInfoRequest path =
    FileInfoRequest { path = path, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting relative paths of the files
contained in the directory at the given path (and any contained directory).

    directoryFullSubPathsRequest : String -> Node.Interface (List String)
    directoryFullSubPathsRequest directoryPath =
        Node.directorySubPathsRequest directoryPath
            |> Node.interfaceFutureRequest
                (\subNamesOrError ->
                    subNamesOrError
                        |> Result.withDefault []
                        |> List.map (\subName -> directoryPath ++ "/" ++ subName)
                )

Uses [`fs.readdir({ recursive: true })`](https://nodejs.org/api/fs.html#fsreaddirpath-options-callback)

-}
directorySubPathsRequest : String -> Interface (Result { code : String, message : String } (List String))
directorySubPathsRequest path =
    DirectorySubPathsRequest { path = path, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for generating a given count of cryptographically sound unsigned 32-bit `Int`s.
You can use these in all kinds of packages that allow creating an initial seed
from ints like [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/Random-Pcg-Extended#initialSeed)

Note: uses [`crypto.getRandomValues`](https://nodejs.org/api/crypto.html#cryptogetrandomvaluestypedarray)

-}
randomUnsignedInt32sRequest : Int -> Interface (List Int)
randomUnsignedInt32sRequest count =
    RandomUnsignedInt32sRequest { count = count, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting when data has been sent from the server at a given address
or the connection has changed, see [`Web.SocketEvent`](Web#SocketEvent).

You also have the ability to send data to the server whenever necessary.

    Node.httpRequestListenAndMaybeRespond
        { portNumber = 1248
        , response =
            case state.pendingRequest of
                Nothing ->
                    Nothing

                Just pendingRequest ->
                    case pendingRequest.data |> Bytes.Decode.decode Bytes.Decode.signedInt32 Bytes.BE of
                        Nothing ->
                            Just
                                { statusCode = 404
                                , headers = []
                                , data = Bytes.Encode.sequence [] |> Bytes.Encode.encode
                                }

                        Just requestNumber ->
                            Just
                                { statusCode = 200
                                , headers = []
                                , data =
                                    (requestNumber * 2)
                                        |> Bytes.Encode.signedInt32 Bytes.BE
                                        |> Bytes.Encode.encode
                                }
        }
        |> Node.interfaceFutureMap
            (\socketEvent ->
                case socketEvent of
                    Node.HttpRequestReceived pendingRequest ->
                        { state | pendingRequest = Just pendingRequest }

                    Node.HttpResponseSent ->
                        { state | pendingRequest = Nothing }

                    Node.HttpServerOpened ->
                        state

                    Node.HttpServerClosed ->
                        state
            )

"Removing" [`Node.httpRequestListenAndMaybeRespond`](#httpRequestListenAndMaybeRespond)
from your interface will close the server at that port.

It's common to send encoded data with [`Json.Encode.encode 0`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Encode#encode)
(usually with the header `{ name = "Content-Type", value = "application/json" }`).

-}
httpRequestListenAndMaybeRespond :
    { portNumber : Int
    , response :
        Maybe
            { statusCode : Int
            , headers : List { name : String, value : String }
            , data : Bytes
            }
    }
    -> Interface HttpServerEvent
httpRequestListenAndMaybeRespond info =
    let
        listenInterface : Interface HttpServerEvent
        listenInterface =
            HttpRequestListen { portNumber = info.portNumber, on = identity }
                |> interfaceFromSingle
    in
    case info.response of
        Nothing ->
            listenInterface

        Just response ->
            [ listenInterface
            , HttpResponseSend
                { portNumber = info.portNumber
                , statusCode = response.statusCode
                , headers = response.headers
                , dataAsciiString = response.data |> AsciiString.fromBytes
                }
                |> interfaceFromSingle
            ]
                |> interfaceBatch


{-| An [`Interface`](Web#Interface) for sending an HTTP request
to a given url

    exampleHttpPost : Web.Interface (Result Web.HttpError Bytes)
    exampleHttpPost =
        Web.httpRequestSend
            { url = ...
            , method = "POST"
            , headers = [ { name = "Content-Type", value = "application/json" } ]
            , body =
                96
                    |> Json.Encode.int
                    |> Json.Encode.encode 0
                    |> Bytes.Encode.encode
                    |> Just
            }

    exampleHttpGet : Web.HttpRequest
    exampleHttpGet =
        Web.httpRequestSend
            { url = ...
            , method = "GET"
            , headers = [ { name = "X-Custom-Header", value = "ProcessThisImmediately" } ]
            , body = Nothing -- empty bytes with HEAD or GET can lead to an HttpError
            }
            |> Web.interfaceFutureMap
                (\responseOrError ->
                    case responseOrError of
                        Ok bytes ->
                            case bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width)) of
                                Just jsonString ->
                                    case jsonString |> Json.Decode.decodeString ... of
                                        Err jsonError ->
                                            Err (jsonError |> Json.Decode.errorToString)

                                        Ok parsed ->
                                            Ok parsed

                                Nothing ->
                                    Err "failed to decode bytes to a string"

                        Err _ ->
                            Err "HTTP response error"
                )

Usually, if you have a request body,
the headers include `Content-Type` with a [MIME type](https://en.wikipedia.org/wiki/Media_type)

The response will either contain [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/).
or an [`HttpError`](Web#HttpError)

If you for some reason need a timeout,
add a [`Web.timeOnceAt`](Web#timeOnceAt) and remove the [`Web.httpRequestSend`](#httpRequestSend)
once it has passed.

Uses the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)

-}
httpRequestSend :
    { url : String
    , method : String
    , headers : List { name : String, value : String }
    , body : Maybe Bytes
    }
    -> Interface (Result HttpError Bytes)
httpRequestSend request =
    { url = request.url
    , method = request.method
    , headers = request.headers
    , bodyAsciiString =
        request.body
            |> Maybe.map AsciiString.fromBytes
    , on = identity
    }
        |> HttpRequestSend
        |> interfaceFromSingle


{-| A Request can fail in a couple ways:

  - `BadUrl`: you did not provide a valid URL
  - `NetworkError`: the user turned off their wifi, went in a cave, etc.
    or the server CORS is misconfigured.
    Note: A 404 for example does not constitute a network error
  - `BadStatus`: you got a response back, but the status code indicates failure. Contains:
      - the statusCode like 404 and statusText describing what the statusCode means a little
      - headers like Content-Length and Expires
      - the body as [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)

-}
type HttpError
    = HttpBadUrl
    | HttpNetworkError String
    | HttpBadStatus
        { statusCode : Int
        , statusText : String
        , headers : List { name : String, value : String }
        , body : Bytes
        }


{-| Read text from standard in.
If you want to intercept input before enter is sent,
use [`Node.standardInRawListen`](#standardInRawListen)

Uses [`process.stdin.addListener("data", ...)`](https://nodejs.org/api/stream.html#event-data)

-}
standardInListen : Interface String
standardInListen =
    StandardInListen identity
        |> interfaceFromSingle


{-| Read text from standard in, regardless of linebreaks.

When used as a terminal interface, user input is read character by character (except modifiers)
as [`StreamDataReceived`](#StreamReadEvent) events.
For nice input parsing,
see for example [gren-tui's `stringToInput`](https://github.com/blaix/gren-tui/blob/main/src/Tui.gren#L562).

As long as [`Node.standardInRawListen`](#standardInRawListen) is part of the interface,
the terminal won't echo input characters.
However, ctrl+c will still exit the process (SIGINT).

Depending on how the final program is used,
input can also arrive from a source outside of the terminal (like a piped file),
in which case the full thing might arrive in multiple [`StreamDataReceived`](#StreamReadEvent)
chunks.

Once all data is send (when outside terminal interface) or CTRL+D is pressed (in terminal interface),
you'll receive a [`StreamDataEndReached`](#StreamReadEvent) event.

Uses [`process.stdin.addListener("data", ...)`](https://nodejs.org/api/stream.html#event-data)
in combination with (when used as a terminal interface) [`process.stdin.setRawMode(true)`](https://nodejs.org/api/tty.html#readstreamsetrawmodemode)

-}
standardInRawListen : Interface StreamReadEvent
standardInRawListen =
    StandardInRawListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for writing text to standard out, like

> hit enter to apply changes or escape to cancel:

If you want to clear the screen, apply color or font effects,
employ [ansi codes](https://dark.elm.dmy.fr/packages/wolfadex/elm-ansi/latest/) instead
(either use a library or go click on the declarations and copy source)

If you find that the last line gets eaten, append a `\n`

Uses [`process.stdout.write`](https://nodejs.org/api/stream.html#writablewritechunk-encoding-callback)

-}
standardOutWrite : String -> Interface future_
standardOutWrite text =
    StandardOutWrite text
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for writing text to standard err.

> input file does not exits. Is the path correct?

Uses [`process.stderr.write`](https://nodejs.org/api/stream.html#writablewritechunk-encoding-callback)

-}
standardErrWrite : String -> Interface future_
standardErrWrite text =
    StandardOutWrite text
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for getting the inner terminal window width and height in characters,
-}
terminalSizeRequest : Interface { lines : Int, columns : Int }
terminalSizeRequest =
    TerminalSizeRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for detecting changes to the inner window width and height
-}
terminalSizeChangeListen : Interface { lines : Int, columns : Int }
terminalSizeChangeListen =
    TerminalSizeChangeListen identity
        |> interfaceFromSingle
