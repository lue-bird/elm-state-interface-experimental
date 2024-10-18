module Node exposing
    ( program, Program
    , Interface, interfaceBatch, interfaceNone, interfaceFutureMap
    , timePosixRequest, timeZoneRequest, timeZoneNameRequest
    , timePeriodicallyListen, timeOnceAt
    , workingDirectoryPathRequest, launchArgumentsRequest, processTitleSet, exit
    , standardOutWrite, standardErrWrite, standardInListen, standardInRawListen
    , terminalSizeRequest, terminalSizeChangeListen
    , FileKind(..), fileInfoRequest, directorySubNamesRequest
    , directoryMake, fileUtf8Write, fileUtf8Request, fileRemove
    , fileChangeListen, FileChange(..)
    , HttpRequest, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , httpRequestSend
    , httpGet, httpPost, httpAddHeaders
    , httpExpectString, httpExpectJson, httpExpectBytes, httpExpectWhatever
    , httpBodyJson, httpBodyBytes
    , httpRequestListenAndMaybeRespond, HttpServerEvent(..)
    , randomUnsignedInt32sRequest
    , ProgramConfig, programInit, programUpdate, programSubscriptions
    , ProgramState(..), ProgramEvent(..), InterfaceSingle(..)
    , interfaceSingleEdits, InterfaceSingleEdit(..)
    )

{-| A state-interface program that can run in [node.js](https://nodejs.org/en)
which is commonly used to create command line tools, scripts and servers.

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
@docs standardOutWrite, standardErrWrite, standardInListen, standardInRawListen
@docs terminalSizeRequest, terminalSizeChangeListen


## file system

@docs FileKind, fileInfoRequest, directorySubNamesRequest
@docs directoryMake, fileUtf8Write, fileUtf8Request, fileRemove
@docs fileChangeListen, FileChange


## HTTP


### client

@docs HttpRequest, HttpBody, HttpExpect, HttpError, HttpMetadata

@docs httpRequestSend
@docs httpGet, httpPost, httpAddHeaders
@docs httpExpectString, httpExpectJson, httpExpectBytes, httpExpectWhatever
@docs httpBodyJson, httpBodyBytes


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

@docs interfaceSingleEdits, InterfaceSingleEdit

If you need more things like json encoders/decoders, [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)

-}

import Bytes exposing (Bytes)
import Bytes.LocalExtra
import Duration exposing (Duration)
import FastDict
import Json.Decode
import Json.Decode.LocalExtra
import Json.Encode
import Json.Encode.LocalExtra
import List.LocalExtra
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
       Unless you want to allow your interface to be edited while it's running,
       it usually contains all the info from the `DomainSubjectVerb` variant (not including functions)

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

    - inside `Node.interfaceSingleEdits`, add a case `[YourName] -> []`.
      If your running interface can be changed, read the section below


   Sometimes, removing + adding the new interface would not be the same as editing the existing one or would at least perform worse.
   For example, changing the volume of an audio should not require removing and and re-adding all audio nodes.

   If you also want to enable editing a running interface:

    - to `Node.InterfaceSingleEdit`, add a variant `| Edit[YourName] ..your diff info..`
    - inside `Node.interfaceSingleEdits`, set the case
      ```elm
      [YourName] old ->
          case case interfaces.updated of
              [YourName] new ->
                  Edit[YourName] ..the diff..
      ```
    - in `runner/node.ts` inside `interfaceEditImplementation`, add a `case "Edit[YourName]" : return (yourInput) => { ... }`
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
    | StandardInRawListen (String -> future)
    | TerminalSizeRequest ({ lines : Int, columns : Int } -> future)
    | TerminalSizeChangeListen ({ lines : Int, columns : Int } -> future)
    | HttpRequestSend (HttpRequest future)
    | HttpRequestListen
        { portNumber : Int
        , on : HttpServerEvent -> future
        }
    | HttpResponseSend
        { portNumber : Int
        , statusCode : Int
        , headers : List { name : String, value : String }
        , data : String
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
    | FileUtf8Write { content : String, path : String }
    | FileUtf8Request { path : String, on : String -> future }
    | FileChangeListen { path : String, on : FileChange -> future }
    | FileInfoRequest
        { path : String
        , on :
            Maybe { byteCount : Int, lastContentChangeTime : Time.Posix, kind : FileKind }
            -> future
        }
    | DirectorySubNamesRequest { path : String, on : List String -> future }


{-| How the connection has changed or what request has been sent.
-}
type HttpServerEvent
    = HttpServerOpened
    | HttpRequestReceived
        { method : String
        , headers : List { name : String, value : String }
        , data : String
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


{-| An HTTP request for use in an [`Interface`](#Interface).

Use [`Node.httpAddHeaders`](Node#httpAddHeaders) to set custom headers as needed.
Use [`Node.timeOnceAt`](Node#timeOnceAt) to add a timeout of how long you are willing to wait before giving up.

-}
type alias HttpRequest future =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List { name : String, value : String }
        , body : HttpBody
        , expect : HttpExpect future
        }


{-| Describe what you expect to be returned in an http response body.
-}
type HttpExpect future
    = HttpExpectString (Result HttpError String -> future)
    | HttpExpectBytes (Result HttpError Bytes -> future)
    | HttpExpectWhatever (Result HttpError () -> future)


{-| Data send in your http request.

  - `HttpBodyEmpty`: Create an empty body for your request.
    This is useful for `GET` requests and `POST` requests where you are not sending any data.

  - `HttpBodyString`: Put a `String` in the body of your request. Defining `Node.httpJsonBody` looks like this:

        import Json.Encode

        httpJsonBody : Json.Encode.Value -> Node.HttpBody
        httpJsonBody value =
            Node.HttpBodyString "application/json" (Json.Encode.encode 0 value)

    The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type) of the body.

  - `HttpBodyUnsignedInt8s` is pretty much the same as `HttpBodyString` but for [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/),
    see [`Node.httpBodyBytes`](Node#httpBodyBytes)

-}
type HttpBody
    = HttpBodyEmpty
    | HttpBodyString { mimeType : String, content : String }
    | HttpBodyUnsignedInt8s { mimeType : String, content : List Int }


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
interfaceFutureMap : (future -> mappedFuture) -> (Interface future -> Interface mappedFuture)
interfaceFutureMap futureChange interface =
    interface
        |> FastDict.map
            (\_ interfaceSingle ->
                interfaceSingle |> interfaceSingleFutureMap futureChange
            )


interfaceSingleFutureMap : (future -> mappedFuture) -> (InterfaceSingle future -> InterfaceSingle mappedFuture)
interfaceSingleFutureMap futureChange interfaceSingle =
    case interfaceSingle of
        HttpRequestSend request ->
            request |> httpRequestFutureMap futureChange |> HttpRequestSend

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

        FileUtf8Write write ->
            FileUtf8Write write

        FileUtf8Request request ->
            FileUtf8Request
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

        DirectorySubNamesRequest request ->
            DirectorySubNamesRequest
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


httpRequestFutureMap : (future -> mappedFuture) -> (HttpRequest future -> HttpRequest mappedFuture)
httpRequestFutureMap futureChange request =
    { url = request.url
    , method = request.method
    , headers = request.headers
    , body = request.body
    , expect =
        case request.expect of
            HttpExpectWhatever expectWhatever ->
                (\unit -> expectWhatever unit |> futureChange) |> HttpExpectWhatever

            HttpExpectString expectString ->
                (\string -> expectString string |> futureChange) |> HttpExpectString

            HttpExpectBytes expectBytes ->
                (\bytes -> expectBytes bytes |> futureChange) |> HttpExpectBytes
    }


{-| The "msg" in a [`Node.program`](#program)
-}
type ProgramEvent appState
    = JsEventFailedToDecode Json.Decode.Error
    | JsEventEnabledConstructionOfNewAppState appState


interfaceSingleEditsMap :
    (InterfaceSingleEdit -> fromSingeEdit)
    ->
        ({ old : InterfaceSingle future, updated : InterfaceSingle future }
         -> List fromSingeEdit
        )
interfaceSingleEditsMap fromSingeEdit interfaces =
    case interfaces.old of
        HttpRequestSend _ ->
            []

        HttpRequestListen _ ->
            []

        HttpResponseSend _ ->
            []

        TimePosixRequest _ ->
            []

        TimezoneOffsetRequest _ ->
            []

        TimeOnce _ ->
            []

        TimePeriodicallyListen _ ->
            []

        TimezoneNameRequest _ ->
            []

        RandomUnsignedInt32sRequest _ ->
            []

        Exit _ ->
            []

        DirectoryMake _ ->
            []

        FileRemove _ ->
            []

        FileUtf8Write _ ->
            []

        FileChangeListen _ ->
            []

        FileUtf8Request _ ->
            []

        FileInfoRequest _ ->
            []

        DirectorySubNamesRequest _ ->
            []

        WorkingDirectoryPathRequest _ ->
            []

        LaunchArgumentsRequest _ ->
            []

        TerminalSizeRequest _ ->
            []

        TerminalSizeChangeListen _ ->
            []

        ProcessTitleSet _ ->
            []

        StandardOutWrite _ ->
            []

        StandardErrWrite _ ->
            []

        StandardInListen _ ->
            []

        StandardInRawListen _ ->
            []


{-| What [`InterfaceSingleEdit`](#InterfaceSingleEdit)s are needed to sync up
-}
interfaceSingleEdits :
    { old : InterfaceSingle future, updated : InterfaceSingle future }
    -> List InterfaceSingleEdit
interfaceSingleEdits interfaces =
    interfaces |> interfaceSingleEditsMap Basics.identity


toJsToJson : { id : String, diff : InterfaceSingleDiff future_ } -> Json.Encode.Value
toJsToJson toJs =
    Json.Encode.object
        [ ( "id", toJs.id |> Json.Encode.string )
        , ( "diff", toJs.diff |> interfaceSingleDiffToJson )
        ]


interfaceSingleDiffToJson : InterfaceSingleDiff future_ -> Json.Encode.Value
interfaceSingleDiffToJson diff =
    Json.Encode.LocalExtra.variant
        (case diff of
            Add interfaceSingleInfo ->
                { tag = "Add", value = interfaceSingleInfo |> interfaceSingleToJson }

            Edit edit ->
                { tag = "Edit", value = edit |> interfaceSingleEditToJson }

            Remove () ->
                { tag = "Remove", value = Json.Encode.null }
        )


interfaceSingleEditToJson : InterfaceSingleEdit -> Json.Encode.Value
interfaceSingleEditToJson edit =
    Json.Encode.LocalExtra.variant
        (case edit of
            EditNoneYet ever ->
                Basics.never ever
        )


interfaceSingleToJson : InterfaceSingle future_ -> Json.Encode.Value
interfaceSingleToJson interfaceSingle =
    Json.Encode.LocalExtra.variant
        (case interfaceSingle of
            HttpRequestSend httpRequestInfo ->
                { tag = "HttpRequestSend", value = httpRequestInfo |> httpRequestInfoToJson }

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
                        , ( "data", send.data |> Json.Encode.string )
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

            FileUtf8Write write ->
                { tag = "FileUtf8Write"
                , value =
                    Json.Encode.object
                        [ ( "path", write.path |> Json.Encode.string )
                        , ( "content", write.content |> Json.Encode.string )
                        ]
                }

            FileUtf8Request request ->
                { tag = "FileUtf8Request"
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

            DirectorySubNamesRequest request ->
                { tag = "DirectorySubNamesRequest"
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


httpRequestInfoToJson : HttpRequest future_ -> Json.Encode.Value
httpRequestInfoToJson httpRequestId =
    Json.Encode.object
        [ ( "url", httpRequestId.url |> Json.Encode.string )
        , ( "method", httpRequestId.method |> Json.Encode.string )
        , ( "headers"
          , httpRequestId.headers
                |> addContentTypeForBody httpRequestId.body
                |> Json.Encode.list
                    (\header ->
                        Json.Encode.object
                            [ ( "name", header.name |> Json.Encode.string )
                            , ( "value", header.value |> Json.Encode.string )
                            ]
                    )
          )
        , ( "expect", httpRequestId.expect |> httpExpectInfoToJson )
        , ( "body", httpRequestId.body |> httpBodyToJson )
        ]


addContentTypeForBody : HttpBody -> (List { name : String, value : String } -> List { name : String, value : String })
addContentTypeForBody body headers =
    case body of
        HttpBodyEmpty ->
            headers

        HttpBodyString stringBodyInfo ->
            { name = "Content-Type", value = stringBodyInfo.mimeType } :: headers

        HttpBodyUnsignedInt8s bytesBodyInfo ->
            { name = "Content-Type", value = bytesBodyInfo.mimeType } :: headers


httpBodyToJson : HttpBody -> Json.Encode.Value
httpBodyToJson body =
    Json.Encode.LocalExtra.variant
        (case body of
            HttpBodyString stringBodyInfo ->
                { tag = "String"
                , value = stringBodyInfo.content |> Json.Encode.string
                }

            HttpBodyUnsignedInt8s bytesBodyInfo ->
                { tag = "Uint8Array"
                , value = bytesBodyInfo.content |> Json.Encode.list Json.Encode.int
                }

            HttpBodyEmpty ->
                { tag = "Empty", value = Json.Encode.null }
        )


httpExpectInfoToJson : HttpExpect future_ -> Json.Encode.Value
httpExpectInfoToJson httpExpectId =
    Json.Encode.string
        (case httpExpectId of
            HttpExpectString _ ->
                "String"

            HttpExpectBytes _ ->
                "Bytes"

            HttpExpectWhatever _ ->
                "Whatever"
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
                        , ( "data", send.data |> StructuredId.ofString )
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

            FileUtf8Write write ->
                { tag = "FileUtf8Write"
                , value =
                    StructuredId.ofParts
                        [ write.path |> StructuredId.ofString
                        , write.content |> StructuredId.ofString
                        ]
                }

            FileUtf8Request request ->
                { tag = "FileUtf8Request"
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

            DirectorySubNamesRequest request ->
                { tag = "DirectorySubNamesRequest"
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
                appConfig.ports.toJs
                    ({ id = id, diff = new |> Add } |> toJsToJson)
                    :: soFar
            )
            []
        |> Cmd.batch
    )


{-| The "subscriptions" part for an embedded program
-}
programSubscriptions : ProgramConfig state -> (ProgramState state -> Sub (ProgramEvent state))
programSubscriptions appConfig (State state) =
    appConfig.ports.fromJs
        (\interfaceJson ->
            interfaceJson
                |> Json.Decode.decodeValue
                    (Json.Decode.field "id" Json.Decode.string
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
                                                    |> FastDict.toList
                                                    |> List.map Tuple.first
                                                    |> String.join "\n"
                                               )
                                            |> Json.Decode.fail
                            )
                        |> Json.Decode.map JsEventEnabledConstructionOfNewAppState
                    )
                |> Result.LocalExtra.valueOrOnError JsEventFailedToDecode
        )


{-| [json `Decoder`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Decoder)
for the transformed event data coming back
-}
interfaceSingleFutureJsonDecoder : InterfaceSingle future -> Maybe (Json.Decode.Decoder future)
interfaceSingleFutureJsonDecoder interface =
    case interface of
        HttpRequestSend request ->
            Json.Decode.oneOf
                [ Json.Decode.LocalExtra.variant "Success" (httpSuccessResponseJsonDecoder request.expect)
                , Json.Decode.LocalExtra.variant "Error" httpErrorJsonDecoder
                    |> Json.Decode.map (httpExpectOnError request.expect)
                ]
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
            Json.Decode.oneOf
                [ Json.Decode.LocalExtra.variant "Ok"
                    (Json.Decode.null (Ok ()))
                , Json.Decode.LocalExtra.variant "Err"
                    (Json.Decode.map2 (\code message -> Err { code = code, message = message })
                        (Json.Decode.field "code"
                            (Json.Decode.oneOf [ Json.Decode.string, Json.Decode.succeed "" ])
                        )
                        (Json.Decode.field "message"
                            (Json.Decode.oneOf [ Json.Decode.string, Json.Decode.succeed "" ])
                        )
                    )
                ]
                |> Json.Decode.map make.on
                |> Just

        FileRemove _ ->
            Nothing

        FileUtf8Write _ ->
            Nothing

        FileUtf8Request request ->
            Json.Decode.string
                |> Json.Decode.map request.on
                |> Just

        FileChangeListen listen ->
            Json.Decode.oneOf
                [ Json.Decode.LocalExtra.variant "Removed"
                    (Json.Decode.map FileRemoved Json.Decode.string)
                , Json.Decode.LocalExtra.variant "AddedOrChanged"
                    (Json.Decode.map FileAddedOrChanged Json.Decode.string)
                ]
                |> Json.Decode.map listen.on
                |> Just

        FileInfoRequest request ->
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
                |> Json.Decode.map request.on
                |> Just

        DirectorySubNamesRequest request ->
            Json.Decode.list Json.Decode.string
                |> Json.Decode.map request.on
                |> Just

        WorkingDirectoryPathRequest on ->
            Json.Decode.string
                |> Json.Decode.map on
                |> Just

        LaunchArgumentsRequest on ->
            Json.Decode.list Json.Decode.string
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
            Json.Decode.string
                |> Json.Decode.map on
                |> Just


httpServerEventJsonDecoder : Json.Decode.Decoder HttpServerEvent
httpServerEventJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.LocalExtra.variant "HttpServerOpened"
            (Json.Decode.null HttpServerOpened)
        , Json.Decode.LocalExtra.variant "HttpRequestReceived"
            (Json.Decode.map3
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
                (Json.Decode.field "data" Json.Decode.string)
            )
        , Json.Decode.LocalExtra.variant "HttpResponseSent"
            (Json.Decode.null HttpResponseSent)
        , Json.Decode.LocalExtra.variant "HttpServerFailed"
            (Json.Decode.map2
                (\code message ->
                    HttpServerFailed { code = code, message = message }
                )
                (Json.Decode.field "code" Json.Decode.string)
                (Json.Decode.field "message" Json.Decode.string)
            )
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


httpExpectOnError : HttpExpect future -> (HttpError -> future)
httpExpectOnError httpExpect e =
    case httpExpect of
        HttpExpectString toFuture ->
            e |> Err |> toFuture

        HttpExpectBytes toFuture ->
            e |> Err |> toFuture

        HttpExpectWhatever toFuture ->
            e |> Err |> toFuture


httpSuccessResponseJsonDecoder : HttpExpect future -> Json.Decode.Decoder future
httpSuccessResponseJsonDecoder expect =
    httpMetadataJsonDecoder
        |> Json.Decode.andThen
            (\meta ->
                let
                    isOk : Bool
                    isOk =
                        meta.statusCode >= 200 && meta.statusCode < 300

                    badStatusJsonDecoder : Json.Decode.Decoder (Result HttpError value_)
                    badStatusJsonDecoder =
                        Json.Decode.map (\body -> Err (HttpBadStatus { metadata = meta, body = body })) Json.Decode.value
                in
                Json.Decode.field "body"
                    (case expect of
                        HttpExpectString toFuture ->
                            Json.Decode.map toFuture
                                (if isOk then
                                    Json.Decode.map Ok Json.Decode.string

                                 else
                                    badStatusJsonDecoder
                                )

                        HttpExpectBytes toFuture ->
                            Json.Decode.map toFuture
                                (if isOk then
                                    Json.Decode.map Ok
                                        (Json.Decode.map Bytes.LocalExtra.fromUnsignedInt8List
                                            (Json.Decode.list Json.Decode.int)
                                        )

                                 else
                                    badStatusJsonDecoder
                                )

                        HttpExpectWhatever toFuture ->
                            Json.Decode.map toFuture
                                (if isOk then
                                    Json.Decode.succeed (Ok ())

                                 else
                                    badStatusJsonDecoder
                                )
                    )
            )


httpMetadataJsonDecoder : Json.Decode.Decoder HttpMetadata
httpMetadataJsonDecoder =
    Json.Decode.map4
        (\url statusCode statusText headers ->
            { url = url
            , statusCode = statusCode
            , statusText = statusText
            , headers = headers
            }
        )
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "statusCode" Json.Decode.int)
        (Json.Decode.field "statusText" Json.Decode.string)
        (Json.Decode.field "headers"
            (Json.Decode.map
                (\headerTuples ->
                    headerTuples
                        |> List.LocalExtra.mapAnyOrder
                            (\( key, value ) -> { key = key, value = value })
                )
                (Json.Decode.keyValuePairs Json.Decode.string)
            )
        )


httpErrorJsonDecoder : Json.Decode.Decoder HttpError
httpErrorJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> HttpBadUrl)
            (Json.Decode.field "cause"
                (Json.Decode.field "code" (Json.Decode.LocalExtra.onlyString "BAD_URL"))
            )
        , Json.Decode.succeed HttpNetworkError
        ]


{-| The "update" part for an embedded program
-}
programUpdate : ProgramConfig state -> (ProgramEvent state -> ProgramState state -> ( ProgramState state, Cmd (ProgramEvent state) ))
programUpdate appConfig event state =
    case event of
        JsEventFailedToDecode jsonError ->
            ( state
            , let
                notifyOfBugInterface : InterfaceSingle never_
                notifyOfBugInterface =
                    ([ "bug: js event failed to decode: "
                     , jsonError |> Json.Decode.errorToString
                     , ". Please open an issue on github.com/lue-bird/elm-state-interface-experimental\n"
                     ]
                        |> String.concat
                    )
                        |> StandardErrWrite
              in
              { id = notifyOfBugInterface |> interfaceSingleToStructuredId |> StructuredId.toString
              , diff = notifyOfBugInterface |> Add
              }
                |> toJsToJson
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
                    (\diff -> appConfig.ports.toJs (diff |> toJsToJson))
                |> Cmd.batch
            )


{-| Determine which outgoing effects need to be executed based on the difference between old and updated interfaces
-}
interfacesDiffMap :
    ({ id : String, diff : InterfaceSingleDiff future } -> combined)
    ->
        ({ old : Interface future
         , updated : Interface future
         }
         -> List combined
        )
interfacesDiffMap idAndDiffCombine interfaces =
    FastDict.merge
        (\removedId _ soFar ->
            idAndDiffCombine { id = removedId, diff = remove } :: soFar
        )
        (\id old updated soFar ->
            List.LocalExtra.appendFast
                ({ old = old, updated = updated }
                    |> interfaceSingleEditsMap
                        (\edit -> idAndDiffCombine { id = id, diff = edit |> Edit })
                )
                soFar
        )
        (\addedId onlyNew soFar ->
            idAndDiffCombine { id = addedId, diff = onlyNew |> Add }
                :: soFar
        )
        interfaces.old
        interfaces.updated
        []


remove : InterfaceSingleDiff irrelevantFuture_
remove =
    Remove ()


{-| A Request can fail in a couple ways:

  - `BadUrl` means you did not provide a valid URL
  - `NetworkError` means the user turned off their wifi, went in a cave, etc.
    or the server CORS is misconfigured.
    Note: A 404 for example does not constitute a network error
  - `BadStatus` means you got a response back, but the status code indicates failure. Contains:
      - The response `Metadata`.
      - The raw response body as a `Json.Decode.Value`.

-}
type HttpError
    = HttpBadUrl
    | HttpNetworkError
    | HttpBadStatus { metadata : HttpMetadata, body : Json.Decode.Value }


{-| Extra information about the response:

  - url of the server that actually responded (so you can detect redirects)
  - statusCode like 200 or 404
  - statusText describing what the statusCode means a little
  - headers like Content-Length and Expires

Note: It is possible for a response to have the same header multiple times.
In that case, all the values end up in a single entry in the headers.
The values are separated by commas, following the rules outlined [here](https://stackoverflow.com/questions/4371328/are-duplicate-http-response-headers-acceptable).

-}
type alias HttpMetadata =
    RecordWithoutConstructorFunction
        { url : String
        , statusCode : Int
        , statusText : String
        , headers : List { key : String, value : String }
        }


{-| Individual message to js to sync up with the latest interface type
-}
type InterfaceSingleDiff irrelevantFuture
    = Add (InterfaceSingle irrelevantFuture)
    | Edit InterfaceSingleEdit
    | Remove ()


{-| Individual message to js to sync up with the latest interface type,
describing changes to an existing interface with the same identity
-}
type InterfaceSingleEdit
    = EditNoneYet Never


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
at a given path with given string content.

Uses [`fs.writeFile`](https://nodejs.org/api/fs.html#fswritefilefile-data-options-callback)

-}
fileUtf8Write : { content : String, path : String } -> Interface future_
fileUtf8Write write =
    FileUtf8Write write
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for reading the string content of the file
at a given path.

Uses [`fs.readFile`](https://nodejs.org/api/fs.html#fsreadfilepath-options-callback)

-}
fileUtf8Request : String -> Interface String
fileUtf8Request path =
    FileUtf8Request { path = path, on = identity }
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


{-| An [`Interface`](Node#Interface) for getting names of the files
contained on the surface in the directory at the given path.

    directorySubPathsRequest : String -> Node.Interface (List String)
    directorySubPathsRequest path =
        Node.fileInfoRequest path
            |> Node.interfaceFutureRequest
                (\subNames ->
                    subNames |> List.map (\subName -> path ++ "/" ++ subName)
                )

Uses [`fs.readdir`](https://nodejs.org/api/fs.html#fsreaddirpath-options-callback)

-}
directorySubNamesRequest : String -> Interface (List String)
directorySubNamesRequest path =
    DirectorySubNamesRequest { path = path, on = identity }
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
                    case pendingRequest.data |> String.fromInt of
                        Nothing ->
                            Just
                                { statusCode = 404
                                , headers = []
                                , data = ""
                                }

                        Just requestNumber ->
                            Just
                                { statusCode = 200
                                , headers = []
                                , data = requestNumber * 2 |> String.fromInt
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
            , data : String
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
                , data = response.data
                }
                |> interfaceFromSingle
            ]
                |> interfaceBatch


{-| Put a given JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
httpBodyJson : Json.Encode.Value -> HttpBody
httpBodyJson content =
    HttpBodyString { mimeType = "application/json", content = Json.Encode.encode 0 content }


{-| Put given [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/) in the body of your request.
The string argument should be a [MIME type](https://en.wikipedia.org/wiki/Media_type) to be used in the `Content-Type` header

    import Bytes exposing (Bytes)
    import Bytes.Encode
    import Node
    import Time
    import Zip
    import Zip.Entry

    exampleZipBody : Node.HttpBody
    exampleZipBody =
        Node.httpBodyBytes "application/zip"
            (Zip.fromEntries
                [ Bytes.Encode.string "Hello, World!"
                    |> Bytes.Encode.encode
                    |> Zip.Entry.store
                        { path = "hello.txt"
                        , lastModified = ( Time.utc, Time.millisToPosix 0 )
                        , comment = Nothing
                        }
                ]
                |> Zip.toBytes
            )

  - 🧩 [`Zip` and `Zip.Entry` are from `agu-z/elm-zip`](https://dark.elm.dmy.fr/packages/agu-z/elm-zip/latest/)

-}
httpBodyBytes : String -> (Bytes -> HttpBody)
httpBodyBytes mimeType content =
    HttpBodyUnsignedInt8s { mimeType = mimeType, content = content |> Bytes.LocalExtra.toUnsignedInt8List }


{-| Expect the response body to be `JSON`, decode it using the given decoder.
The result will either be

  - `Err` with an [`HttpError`](Node#HttpError) if it didn't succeed
  - `Ok` if there was a result with either
      - `Ok` with the decoded value
      - `Err` with a [`Json.Decode.Error`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Error)
        and the actual text response

-}
httpExpectJson :
    Json.Decode.Decoder future
    ->
        HttpExpect
            (Result
                HttpError
                (Result { actualBody : String, jsonError : Json.Decode.Error } future)
            )
httpExpectJson stateDecoder =
    HttpExpectString
        (\result ->
            case result of
                Ok jsonString ->
                    case jsonString |> Json.Decode.decodeString stateDecoder of
                        Err jsonError ->
                            Ok (Err { actualBody = jsonString, jsonError = jsonError })

                        Ok json ->
                            Ok (Ok json)

                Err httpError ->
                    Err httpError
        )


{-| Expect the response body to be [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/).
The result will either be

  - `Err` with an [`HttpError`](Node#HttpError) if it didn't succeed
  - `Ok` with the `Bytes`

-}
httpExpectBytes : HttpExpect (Result HttpError Bytes)
httpExpectBytes =
    HttpExpectBytes identity


{-| Expect the response body to be a `String`.
-}
httpExpectString : HttpExpect (Result HttpError String)
httpExpectString =
    HttpExpectString identity


{-| Discard the response body.
-}
httpExpectWhatever : HttpExpect (Result HttpError ())
httpExpectWhatever =
    HttpExpectWhatever identity


{-| Create a `GET` [`HttpRequest`](Node#HttpRequest).

Use [`Node.httpAddHeaders`](Node#httpAddHeaders) to set custom headers as needed.
Use [`Node.timeOnceAt`](Node#timeOnceAt) to add a timeout of how long you are willing to wait before giving up.

-}
httpGet :
    { url : String
    , expect : HttpExpect future
    }
    -> HttpRequest future
httpGet options =
    { url = options.url
    , method = "GET"
    , headers = []
    , body = HttpBodyEmpty
    , expect = options.expect
    }


{-| Add custom headers to the [`Node.HttpRequest`](Node#HttpRequest).

    request
        |> Node.httpAddHeaders
            [ ( "X-Custom-Header", "ProcessThisImmediately" )
            ]

-}
httpAddHeaders : List ( String, String ) -> (HttpRequest future -> HttpRequest future)
httpAddHeaders headers request =
    { request
        | headers =
            (headers |> List.map (\( name, value ) -> { name = name, value = value }))
                ++ request.headers
    }


{-| Create a `POST` [`HttpRequest`](Node#HttpRequest).

Use [`Node.httpAddHeaders`](Node#httpAddHeaders) to set custom headers as needed.
Use [`Node.timeOnceAt`](Node#timeOnceAt) to add a timeout of how long you are willing to wait before giving up.

-}
httpPost :
    { url : String
    , body : HttpBody
    , expect : HttpExpect future
    }
    -> HttpRequest future
httpPost options =
    { url = options.url
    , method = "POST"
    , headers = []
    , body = options.body
    , expect = options.expect
    }


{-| An [`Interface`](Node#Interface) for handling an [`HttpRequest`](Node#HttpRequest)
using the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
-}
httpRequestSend : HttpRequest future -> Interface future
httpRequestSend request =
    HttpRequestSend request
        |> interfaceFromSingle


{-| Read text from standard in.
If you want to intercept input before enter is sent,
use [`Node.standardInRawListen`](#standardInRawListen)

Uses [`process.stdin.addListener("data", ...)`](https://nodejs.org/api/stream.html#event-data)

-}
standardInListen : Interface String
standardInListen =
    StandardInListen identity
        |> interfaceFromSingle


{-| Read text from standard in character by character (except modifiers).
For nice input parsing,
see for example [gren-tui's `stringToInput`](https://github.com/blaix/gren-tui/blob/main/src/Tui.gren#L562).

As long as [`Node.standardInRawListen`](#standardInRawListen) is part of the interface,
the terminal won't echo input characters.
However, ctrl+c will still exit the process (SIGINT).

Uses [`process.stdin.addListener("data", ...)`](https://nodejs.org/api/stream.html#event-data)
in combination with [`process.stdin.setRawMode(true)`](https://nodejs.org/api/tty.html#readstreamsetrawmodemode)

-}
standardInRawListen : Interface String
standardInRawListen =
    StandardInRawListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Node#Interface) for writing text to standard out, like

> hit enter to apply changes or escape to cancel:

If you want to clear the screen, apply color or font effects,
employ [ansi codes](https://dark.elm.dmy.fr/packages/wolfadex/elm-ansi/latest/) instead
(either use a library or go click on the declarations and copy source)

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
