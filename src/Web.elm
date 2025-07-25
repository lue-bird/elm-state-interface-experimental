module Web exposing
    ( program, Program
    , Interface, interfaceBatch, interfaceNone, interfaceFutureMap
    , timePosixRequest, timeZoneRequest, timeZoneNameRequest
    , timePeriodicallyListen, timeOnceAt
    , domText, domElement, svgElement, domElementNamespaced, DomNode(..)
    , domFutureMap, domRender
    , DomModifier, domModifierFutureMap, domModifierBatch, domModifierNone
    , domAttribute, domAttributeNamespaced, domStyle, domBoolProperty, domStringProperty
    , domListenTo, domListenToPreventingDefaultAction
    , domScrollToShow, DomElementVisibilityAlignment(..), domScrollPositionRequest, domScrollToPosition
    , animationFrameListen, windowVisibilityChangeListen, WindowVisibility(..)
    , windowSizeRequest, windowSizeChangeListen
    , preferredLanguagesRequest, preferredLanguagesChangeListen
    , documentListenTo, windowListenTo
    , mediaQueryRequest, mediaQueryChangeListen
    , titleReplaceBy, authorSet, keywordsSet, descriptionSet
    , urlRequest
    , urlPush, urlReplace
    , navigateForward, navigateBack, navigationListen
    , navigateTo, reload
    , httpRequestSend, HttpError(..)
    , randomUnsignedInt32sRequest
    , consoleLog, consoleWarn, consoleError
    , localStorageRequest, localStorageSet, localStorageRemove
    , localStorageSetOnADifferentTabListen, localStorageRemoveOnADifferentTabListen
    , SocketEvent(..), socketListenAndMaybeSend
    , fileDownloadBytes
    , Audio, AudioSource, AudioSourceLoadError(..), AudioProcessing(..), AudioParameterTimeline
    , audioSourceLoad, audioFromSource, audioPlay
    , audioVolumeScaleBy, audioSpeedScaleBy, audioStereoPan
    , audioAddLinearConvolutionWith, audioAddHighpassFromFrequency, audioAddLowpassUntilFrequency
    , audioParameterAt, audioParameterThrough
    , clipboardRequest, clipboardReplaceBy
    , GeoLocation, geoLocationRequest, geoLocationChangeListen
    , Gamepad, GamepadButton(..), gamepadsRequest, gamepadsChangeListen
    , notificationAskForPermission, notificationShow, NotificationClicked(..)
    , ProgramConfig, programInit, programUpdate, programSubscriptions
    , DomElement, DefaultActionHandling(..), DomModifierSingle(..)
    , ProgramState(..), ProgramEvent(..), InterfaceSingle(..), DomTextOrElementHeader(..), DomElementHeader
    , SortedKeyValueList(..)
    )

{-| A state-interface program that can run in the browser

@docs program, Program

You can also [embed](#embed) a state-interface program as part of an existing app that uses The Elm Architecture.


# interface

@docs Interface, interfaceBatch, interfaceNone, interfaceFutureMap


## time

See [`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/)

@docs timePosixRequest, timeZoneRequest, timeZoneNameRequest
@docs timePeriodicallyListen, timeOnceAt


## DOM

Primitives used for SVG and HTML
(filling the same role as [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/))

@docs domText, domElement, svgElement, domElementNamespaced, DomNode
@docs domFutureMap, domRender
@docs DomModifier, domModifierFutureMap, domModifierBatch, domModifierNone
@docs domAttribute, domAttributeNamespaced, domStyle, domBoolProperty, domStringProperty
@docs domListenTo, domListenToPreventingDefaultAction
@docs domScrollToShow, DomElementVisibilityAlignment, domScrollPositionRequest, domScrollToPosition


## window

Observe and alter the page's global environment

@docs animationFrameListen, windowVisibilityChangeListen, WindowVisibility
@docs windowSizeRequest, windowSizeChangeListen
@docs preferredLanguagesRequest, preferredLanguagesChangeListen
@docs documentListenTo, windowListenTo
@docs mediaQueryRequest, mediaQueryChangeListen

When navigating to a new page on the same site,
you may want to change the document's context:

@docs titleReplaceBy, authorSet, keywordsSet, descriptionSet


## navigation

`history` interaction

@docs urlRequest
@docs urlPush, urlReplace
@docs navigateForward, navigateBack, navigationListen
@docs navigateTo, reload


## HTTP client

@docs httpRequestSend, HttpError


## random

Not familiar with random "generators"? [`elm/random`](https://package.elm-lang.org/packages/elm/random/latest)
explains it nicely!

Here's an example showing a number between 1 and 6 and a button to reroll
using [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/)

    import Random.Pcg.Extended
    import Web

    type State
        = WaitingForInitialRandomness
        | DiceUiState { diceEyes : Int, seed : Random.Pcg.Extended.Seed }

    type DiceUiEvent
        = RerollClicked

    diceEyesRandomGenerator : Random.Pcg.Extended.Generator Int
    diceEyesRandomGenerator =
        Random.Pcg.Extended.int 1 6

    { initialState = WaitingForInitialRandomness
    , interface =
        \state ->
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
                        []
                        [ randomStuff.diceEyes |> String.fromInt |> Web.domText
                        , Web.domElement "button"
                            [ Web.domListenTo "click"
                                |> Web.domModifierFutureMap (\_ -> RerollClicked)
                            ]
                            [ Web.domText "roll the dice" ]
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
    }

@docs randomUnsignedInt32sRequest


## console

@docs consoleLog, consoleWarn, consoleError


## local storage

Saved data for the url origin (protocol, host name, port) across browser sessions.

This data doesn't expire and won't be cleared when the page is closed.
The only exception is "incognito mode", where all data is cleared once the last "private" tab is closed.

see [mdn on `Window.localStorage`](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage)

@docs localStorageRequest, localStorageSet, localStorageRemove
@docs localStorageSetOnADifferentTabListen, localStorageRemoveOnADifferentTabListen


## web sockets

@docs SocketEvent, socketListenAndMaybeSend


## file download

Downloading a dynamically generated file.

Security note: Browsers require downloads to be initiated by a user event.
So rather than allowing malicious sites to put files on your computer however they please,
the user has to at least click a button first.
As a result, the following interfaces only work when they are triggered by some user event.

Note: There's no equivalent module for file select
since you can easily replicate the behavior using an input element with type file or file drop area modifiers,
see for example [mpizenberg/elm-file](https://dark.elm.dmy.fr/packages/mpizenberg/elm-file/latest/FileValue#load-files-with-an-input).

@docs fileDownloadBytes


## audio

Play sounds and music.

    import Time
    import Web

    type alias State =
        { audioSource : Maybe (Result Web.AudioSourceLoadError Web.AudioSource)
        , audioStartTime : Maybe Time.Posix
        }

    { initialState =
        { audioSource = Nothing
        , audioStartTime = Nothing
        }
    , interface =
        \state ->
            case state.audioSource of
                Just (Ok audioSource) ->
                    case state.audioStartTime of
                        Just startTime
                            Web.audioFromSource audioSource startTime
                                |> Web.audioPlay


                        Nothing ->
                            Web.timePosixRequest
                                |> Web.interfaceFutureMap
                                    (\time -> { state | audioSTartTime = time |> Just })

                Nothing ->
                    Web.audioSourceLoad "https://s3-us-west-2.amazonaws.com/s.cdpn.io/123941/Yodel_Sound_Effect.mp3"
                        |> Web.interfaceFutureMap
                            (\result -> { state | audioSource = result |> Just })

                Just (Err _) ->
                    Web.consoleError "audio failed to load"
    }

@docs Audio, AudioSource, AudioSourceLoadError, AudioProcessing, AudioParameterTimeline

@docs audioSourceLoad, audioFromSource, audioPlay
@docs audioVolumeScaleBy, audioSpeedScaleBy, audioStereoPan
@docs audioAddLinearConvolutionWith, audioAddHighpassFromFrequency, audioAddLowpassUntilFrequency

To detune, use [`audioSpeedScaleBy`](#audioSpeedScaleBy). It's documentation also shows which scale relates to which semitone pitch.

To build an [`AudioParameterTimeline`](Web#AudioParameterTimeline):

@docs audioParameterAt, audioParameterThrough


## clipboard

@docs clipboardRequest, clipboardReplaceBy

Note: To listen for [copy, cut and paste events](https://developer.mozilla.org/en-US/docs/Web/API/ClipboardEvent),
use [`Web.domListenTo`](Web#domListenTo)


## geo location

Observe the [`GeoLocation`](Web#GeoLocation)
using the [web geolocation API](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation_API).

@docs GeoLocation, geoLocationRequest, geoLocationChangeListen


## gamepads

Observe connected [gamepads and other game controllers](Web#Gamepad)
(not including motion sensing, gesture recognition etc.).

    import Web

    interface =
        \state ->
            [ Web.timePeriodicallyListen (Duration.seconds (1 / 50))
                |> Web.interfaceFutureMap
                    ..simulate one tick using gamepad inputs..
            , [ Web.gamepadsRequest, Web.gamepadsChangeListen ]
                |> Web.interfaceBatch
                |> Web.interfaceFutureMap (\gamepads -> { state | gamepads = gamepads })
            ]
                |> Web.interfaceBatch

If your gamepad isn't showing up in the list,
press some buttons. On some devices, only certain buttons will wake up the gamepad API (the shapes on PS3 controllers, for instance)

@docs Gamepad, GamepadButton, gamepadsRequest, gamepadsChangeListen


## notification

Give important notices to the user as push notifications.
Consider this a convenience feature, not something users have to rely upon.
Always offer users alternative methods to view messages or initiate actions
and allow users to opt out of getting more in the future.

@docs notificationAskForPermission, notificationShow, NotificationClicked

You can combine it with [`Web.windowVisibilityChangeListen`](Web#windowVisibilityChangeListen)
to only notify users when they're on a different page

    import Web

    type State
        = State
            { windowVisibility : Web.WindowVisibility
            , whoseMove : Player
            , mode : Mode
            , notificationPermissionToggle : Permission
            }

    type Permission
        = Rejected
        | Accepted

    type Mode
        = LongGameBoardMode
        | SettingsPage

    type Player
        = You
        | Opponent

    interface : State -> Web.Interface State
    interface =
        \(State state) ->
            [ case state.notificationPermissionToggle of
                Accepted ->
                    Web.notificationAskForPermission

                Rejected ->
                    Web.interfaceNone
            , case state.windowVisibility of
                Web.WindowShown ->
                    Web.interfaceNone

                Web.WindowHidden ->
                    case state.whoseTurn of
                        Opponent ->
                            Web.interfaceNone

                        You ->
                            Web.Notification.show
                                { message = "opponent moved", ... }
                                |> Web.interfaceFutureMap
                                    (\Web.NotificationClicked ->
                                        -- return to the game if previously in settings
                                        State { state | mode = LongGameBoardMode }
                                    )

            , case state.mode of
                LongGameBoardMode ->
                    ..listen for opponent move from server..
                        |> Web.interfaceFutureMap
                            (\... -> State { state | whoseMove = You })

                SettingsPage ->
                    ..toggle for accepting/rejecting notifications..
                        |> Web.interfaceFutureMap
                            (\... -> State { state | notificationPermissionToggle = ..opposite.. })
            ]
                |> Web.interfaceBatch



## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs ProgramConfig, programInit, programUpdate, programSubscriptions

Under the hood, [`Web.program`](Web#program) is then defined as just

    program config =
        Platform.worker
            { init = \() -> Web.programInit yourAppConfig
            , update = Web.programUpdate yourAppConfig
            , subscriptions = Web.programSubscriptions yourAppConfig
            }


## internals, safe to ignore for users

Exposed so can for example simulate it more easily in tests, add a debugger etc.

@docs DomElement, DefaultActionHandling, DomModifierSingle

@docs ProgramState, ProgramEvent, InterfaceSingle, DomTextOrElementHeader, DomElementHeader

@docs SortedKeyValueList

If you need more things like json encoders/decoders, [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)

-}

import Angle exposing (Angle)
import AppUrl exposing (AppUrl)
import AsciiString
import Bytes exposing (Bytes)
import Dict
import Duration exposing (Duration)
import FastDict
import Json.Decode
import Json.Decode.LocalExtra
import Json.Encode
import Json.Encode.LocalExtra
import Length exposing (Length)
import List.LocalExtra
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Speed exposing (Speed)
import StructuredId exposing (StructuredId)
import Time
import Time.LocalExtra
import Url exposing (Url)



{- Hey! Here's how to add a new interface:


     - choose a name with roughly the shape `DomainSubjectVerb`

     - to `Web.InterfaceSingle`, add a variant `| [YourName] ..additional info (and future handles)..`

     - inside `Web.interfaceSingleFutureJsonDecoder`, specify what js values you expect to decode

     - inside `Web.interfaceSingleToStructuredId`, assign a unique identifier to your interface

       This helps recognize when interfaces have been added, have changed or have been deleted.
       Unless you want to allow your interface to be edited while it's running,
       it usually contains all the info from the `DomainSubjectVerb` variant (not including functions)

     - in `runner/web.ts` inside `interfaceAddImplementation`, add
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

    - inside `Web.interfaceSingleEditsMap`, add a case `[YourName] -> []`.
      If your running interface can be changed, read the section below


   Sometimes, removing + adding the new interface would not be the same as editing the existing one or would at least perform worse.
   For example, changing the volume of an audio should not require removing and and re-adding all audio nodes.

   If you also want to enable editing a running interface:

    - to `Web.InterfaceSingleEdit`, add a variant `| Edit[YourName] ..your diff info..`
    - inside `Web.interfaceSingleEditsMap`, set the case
      ```elm
      [YourName] old ->
          case case interfaces.updated of
              [YourName] new ->
                  Edit[YourName] ..the diff..
      ```
    - in `runner/web.ts` inside `interfaceEditImplementation`, add a `case "Edit[YourName]" : return (yourInput) => { ... }`
-}


{-| The "model" in a [`Web.program`](#program)
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
To create one, use the helpers in [time](#time), [DOM](#dom), [HTTP](#http-client) etc.

To combine multiple, use [`Web.interfaceBatch`](#interfaceBatch) and [`Web.interfaceNone`](#interfaceNone).
To change the value that comes back in the future, use [`Web.interfaceFutureMap`](Web#interfaceFutureMap)

-}
type alias Interface future =
    FastDict.Dict String (InterfaceSingle future)


{-| A "non-batched" [`Interface`](#Interface).
To create one, use the helpers in [time](#time), [DOM](#dom), [HTTP](#http-client) etc.
-}
type InterfaceSingle future
    = DocumentTitleReplaceBy String
    | DocumentAuthorSet String
    | DocumentKeywordsSet (List String)
    | DocumentDescriptionSet String
    | DocumentEventListen { eventName : String, on : Json.Decode.Decoder future }
    | ConsoleLog String
    | ConsoleWarn String
    | ConsoleError String
    | NavigationReplaceUrl AppUrl
    | NavigationPushUrl AppUrl
    | NavigationGo Int
    | NavigationLoad String
    | NavigationReload ()
    | NavigationUrlRequest (AppUrl -> future)
    | FileDownload { mimeType : String, name : String, contentAsciiString : String }
    | ClipboardReplaceBy String
    | ClipboardRequest (String -> future)
    | AudioSourceLoad { url : String, on : Result AudioSourceLoadError AudioSource -> future }
    | AudioPlay Audio
    | DomNodeRender (DomNode future)
    | NotificationAskForPermission ()
    | NotificationShow { id : String, message : String, details : String, on : NotificationClicked -> future }
    | HttpRequestSend
        { url : String
        , method : String
        , headers : List { name : String, value : String }
        , bodyAsciiString : Maybe String
        , on : Result HttpError Bytes -> future
        }
    | TimePosixRequest (Time.Posix -> future)
    | TimezoneOffsetRequest (Int -> future)
    | TimeOnce { pointInTime : Time.Posix, on : Time.Posix -> future }
    | TimePeriodicallyListen { intervalDurationMilliSeconds : Int, on : Time.Posix -> future }
    | TimezoneNameRequest (String -> future)
    | RandomUnsignedInt32sRequest { count : Int, on : List Int -> future }
    | WindowSizeRequest ({ width : Int, height : Int } -> future)
    | WindowPreferredLanguagesRequest (List String -> future)
    | WindowEventListen { eventName : String, on : Json.Decode.Decoder future }
    | WindowVisibilityChangeListen (WindowVisibility -> future)
    | WindowAnimationFrameListen (Time.Posix -> future)
    | WindowPreferredLanguagesChangeListen (List String -> future)
    | MediaQueryRequest { queryString : String, on : Bool -> future }
    | MediaQueryChangeListen { queryString : String, on : Bool -> future }
    | SocketListen
        { address : String
        , on : SocketEvent -> future
        }
    | SocketDataSend { address : String, data : String }
    | LocalStorageSet { key : String, value : Maybe String }
    | LocalStorageRequest { key : String, on : Maybe String -> future }
    | LocalStorageRemoveOnADifferentTabListen { key : String, on : AppUrl -> future }
    | LocalStorageSetOnADifferentTabListen
        { key : String
        , on : { appUrl : AppUrl, oldValue : Maybe String, newValue : String } -> future
        }
    | GeoLocationRequest (GeoLocation -> future)
    | GeoLocationChangeListen (GeoLocation -> future)
    | GamepadsRequest (Dict.Dict Int Gamepad -> future)
    | GamepadsChangeListen (Dict.Dict Int Gamepad -> future)


{-| These are possible errors we can get when loading an audio source file.

  - `AudioSourceLoadDecodeError`: This means we got the data but we couldn't decode it. One likely reason for this is that your url points to the wrong place and you're trying to decode a 404 page instead.
  - `AudioSourceLoadNetworkError`: We couldn't reach the url. Either it's some kind of CORS issue, the server is down, or you're disconnected from the internet.

-}
type AudioSourceLoadError
    = AudioSourceLoadDecodeError
    | AudioSourceLoadNetworkError


{-| The user clicked a displayed notification,
moving the focus to our page
-}
type NotificationClicked
    = NotificationClicked


{-| How the connection has changed or what message has been sent
-}
type SocketEvent
    = SocketOpened
    | SocketDataReceived String
    | SocketClosed { code : Int, reason : String }


{-| Position and (if available) altitude of the device on Earth, as well as the accuracy with which these properties are calculated.
The geographic position information is provided in terms of World Geodetic System coordinates (WGS84).

Device movement direction and speed might also be provided.

[`Length`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Length),
[`Angle`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Angle) and
[`Speed`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Speed)
are from `ianmackenzie/elm-units`

-}
type alias GeoLocation =
    RecordWithoutConstructorFunction
        { latitudeInDecimalDegrees : Float
        , longitudeInDecimalDegrees : Float
        , latitudeLongitudeAccuracy : Maybe Length
        , altitudeAboveNominalSeaLevel : Maybe Length
        , altitudeAccuracy : Maybe Length
        , headingWith0AsTrueNorthAndIncreasingClockwise : Maybe Angle
        , speed : Maybe Speed
        }


{-| The visibility to the user

  - `WindowHidden`: the user has navigated to a new page, switched tabs, closed the tab, minimized or closed the browser, or, on mobile, switched from the browser to a different app
  - `WindowShown` otherwise

-}
type WindowVisibility
    = WindowShown
    | WindowHidden


{-| Plain text or a [`DomElementHeader`](#DomElementHeader) for use in an [`Interface`](#Interface).
-}
type DomTextOrElementHeader future
    = DomHeaderText String
    | DomElementHeader (DomElementHeader future)


{-| Everything about a [tagged DOM element](Web#DomElement)
except potential sub-[node](Web#DomNode)s
-}
type alias DomElementHeader future =
    RecordWithoutConstructorFunction
        { namespace : Maybe String
        , tag : String
        , styles : SortedKeyValueList String String
        , attributes : SortedKeyValueList String String
        , attributesNamespaced : SortedKeyValueList { namespace : String, key : String } String
        , stringProperties : SortedKeyValueList String String
        , boolProperties : SortedKeyValueList String Bool
        , scrollToPosition : Maybe { fromLeft : Float, fromTop : Float }
        , scrollToShow : Maybe { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment }
        , scrollPositionRequest : Maybe ({ fromLeft : Float, fromTop : Float } -> future)
        , eventListens :
            SortedKeyValueList
                String
                { on : Json.Decode.Value -> future
                , defaultActionHandling : DefaultActionHandling
                }
        }


{-| What part of the [`Web.domElement`](Web#domElement) should be visible

  - `DomElementStart`: mostly for text to read
  - `DomElementEnd`: mostly for text to write
  - `DomElementCenter`: mostly for images

-}
type DomElementVisibilityAlignment
    = DomElementStart
    | DomElementEnd
    | DomElementCenter


{-| Setting for a listen [`Web.DomModifier`](Web#DomModifier)
to keep or overwrite the browser's default action
-}
type DefaultActionHandling
    = DefaultActionPrevent
    | DefaultActionExecute


{-| Combine multiple [`Interface`](#Interface)s into one
-}
interfaceBatch : List (Interface future) -> Interface future
interfaceBatch interfaces =
    interfaces |> List.foldl interfaceBatch2 FastDict.empty


{-| Combine 2 [`Interface`](#Interface)s into one.
Equivalent to [`interfaceBatch [ a, b ]`](#interfaceBatch) but faster.
-}
interfaceBatch2 : Interface future -> Interface future -> Interface future
interfaceBatch2 =
    FastDict.union


{-| Doing nothing as an [`Interface`](#Interface). These two examples are equivalent:

    Web.interfaceBatch [ a, Web.interfaceNone, b ]

and

    Web.interfaceBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
interfaceNone : Interface future_
interfaceNone =
    FastDict.empty


{-| Take what the [`Interface`](#Interface) can come back with and return a different future value.

In practice, this is sometimes used like a kind of event-config pattern:

    Web.timePosixRequest
        |> Web.interfaceFutureMap (\timeNow -> TimeReceived timeNow)

    button "show all entries"
        |> Web.domRender
        |> Web.interfaceFutureMap (\Pressed -> ShowAllEntriesButtonClicked)

sometimes as a way to deal with all events (like `update` in The Elm Architecture)

    ...
        |> Web.interfaceFutureMap
            (\event ->
                case event of
                    MouseMovedTo newMousePoint ->
                        { state | mousePoint = newMousePoint }

                    CounterDecreaseClicked ->
                        { state | counter = state.counter - 1 }

                    CounterIncreaseClicked ->
                        { state | counter = state.counter + 1 }
            )

and sometimes to nest events (like `Cmd.map/Task.map/Sub.map/...` in The Elm Architecture):

    type Event
        = DirectoryTreeViewEvent TreeUiEvent
        | SortButtonClicked

    type TreeUiEvent
        = Expanded TreePath
        | Collapsed TreePath

    interface : State -> Interface State
    interface state =
        ...
            [ treeUi ..
                |> Web.interfaceFutureMap DirectoryTreeViewEvent
            , ...
            ]
            |> Web.domRender

    treeUi : ... -> Web.DomNode TreeUiEvent

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
        DocumentTitleReplaceBy title ->
            DocumentTitleReplaceBy title

        DocumentAuthorSet author ->
            DocumentAuthorSet author

        DocumentKeywordsSet keywords ->
            DocumentKeywordsSet keywords

        DocumentDescriptionSet description ->
            DocumentDescriptionSet description

        ConsoleLog message ->
            ConsoleLog message

        ConsoleWarn message ->
            ConsoleWarn message

        ConsoleError message ->
            ConsoleError message

        NavigationReplaceUrl appUrl ->
            NavigationReplaceUrl appUrl

        NavigationPushUrl appUrl ->
            NavigationPushUrl appUrl

        NavigationGo urlStepCount ->
            NavigationGo urlStepCount

        NavigationLoad url ->
            NavigationLoad url

        NavigationReload () ->
            navigationReload

        FileDownload download ->
            FileDownload download

        ClipboardReplaceBy clipboard ->
            ClipboardReplaceBy clipboard

        AudioPlay audio ->
            AudioPlay audio

        SocketListen listen ->
            SocketListen
                { address = listen.address
                , on = \event -> listen.on event |> futureChange
                }

        SocketDataSend send ->
            SocketDataSend send

        LocalStorageSet localStorageItem ->
            LocalStorageSet localStorageItem

        NotificationAskForPermission () ->
            notificationAskForPermissionSingle

        DomNodeRender domNode ->
            (domNode |> domFutureMap futureChange)
                |> DomNodeRender

        AudioSourceLoad sourceLoad ->
            { url = sourceLoad.url, on = \event -> sourceLoad.on event |> futureChange }
                |> AudioSourceLoad

        NotificationShow show ->
            { id = show.id
            , message = show.message
            , details = show.details
            , on = \future -> future |> show.on |> futureChange
            }
                |> NotificationShow

        HttpRequestSend send ->
            { url = send.url
            , method = send.method
            , headers = send.headers
            , bodyAsciiString = send.bodyAsciiString
            , on = \responseBytes -> send.on responseBytes |> futureChange
            }
                |> HttpRequestSend

        LocalStorageRequest request ->
            { key = request.key, on = \event -> event |> request.on |> futureChange }
                |> LocalStorageRequest

        WindowSizeRequest toFuture ->
            (\event -> toFuture event |> futureChange) |> WindowSizeRequest

        WindowPreferredLanguagesRequest toFuture ->
            (\event -> toFuture event |> futureChange) |> WindowPreferredLanguagesRequest

        NavigationUrlRequest toFuture ->
            (\event -> toFuture event |> futureChange) |> NavigationUrlRequest

        ClipboardRequest toFuture ->
            (\event -> toFuture event |> futureChange) |> ClipboardRequest

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

        GeoLocationRequest toFuture ->
            (\event -> event |> toFuture |> futureChange) |> GeoLocationRequest

        GamepadsRequest toFuture ->
            (\event -> event |> toFuture |> futureChange) |> GamepadsRequest

        WindowEventListen listen ->
            { eventName = listen.eventName, on = listen.on |> Json.Decode.map futureChange }
                |> WindowEventListen

        WindowVisibilityChangeListen toFuture ->
            (\event -> toFuture event |> futureChange) |> WindowVisibilityChangeListen

        WindowAnimationFrameListen toFuture ->
            (\event -> toFuture event |> futureChange) |> WindowAnimationFrameListen

        WindowPreferredLanguagesChangeListen toFuture ->
            (\event -> toFuture event |> futureChange) |> WindowPreferredLanguagesChangeListen

        MediaQueryChangeListen listen ->
            { queryString = listen.queryString
            , on = \event -> listen.on event |> futureChange
            }
                |> MediaQueryChangeListen

        MediaQueryRequest request ->
            { queryString = request.queryString
            , on = \event -> request.on event |> futureChange
            }
                |> MediaQueryRequest

        TimePeriodicallyListen periodicallyListen ->
            { intervalDurationMilliSeconds = periodicallyListen.intervalDurationMilliSeconds
            , on = \posix -> periodicallyListen.on posix |> futureChange
            }
                |> TimePeriodicallyListen

        DocumentEventListen listen ->
            { eventName = listen.eventName, on = listen.on |> Json.Decode.map futureChange }
                |> DocumentEventListen

        LocalStorageRemoveOnADifferentTabListen listen ->
            { key = listen.key, on = \event -> event |> listen.on |> futureChange }
                |> LocalStorageRemoveOnADifferentTabListen

        LocalStorageSetOnADifferentTabListen listen ->
            { key = listen.key, on = \event -> event |> listen.on |> futureChange }
                |> LocalStorageSetOnADifferentTabListen

        GeoLocationChangeListen toFuture ->
            (\event -> event |> toFuture |> futureChange) |> GeoLocationChangeListen

        GamepadsChangeListen toFuture ->
            (\event -> event |> toFuture |> futureChange) |> GamepadsChangeListen


navigationReload : InterfaceSingle future_
navigationReload =
    NavigationReload ()


notificationAskForPermissionSingle : InterfaceSingle future_
notificationAskForPermissionSingle =
    NotificationAskForPermission ()


domElementHeaderFutureMap : (future -> mappedFuture) -> DomElementHeader future -> DomElementHeader mappedFuture
domElementHeaderFutureMap futureChange domElementToMap =
    { namespace = domElementToMap.namespace
    , tag = domElementToMap.tag
    , styles = domElementToMap.styles
    , attributes = domElementToMap.attributes
    , attributesNamespaced = domElementToMap.attributesNamespaced
    , stringProperties = domElementToMap.stringProperties
    , boolProperties = domElementToMap.boolProperties
    , scrollToPosition = domElementToMap.scrollToPosition
    , scrollToShow = domElementToMap.scrollToShow
    , scrollPositionRequest =
        domElementToMap.scrollPositionRequest
            |> Maybe.map (\request position -> position |> request |> futureChange)
    , eventListens =
        domElementToMap.eventListens
            |> sortedKeyValueListMap
                (\listen ->
                    { on = \event -> listen.value.on event |> futureChange
                    , defaultActionHandling = listen.value.defaultActionHandling
                    }
                )
    }


sortedKeyValueListMap :
    ({ key : key, value : value } -> newValue)
    -> SortedKeyValueList key value
    -> SortedKeyValueList key newValue
sortedKeyValueListMap elementChange (SortedKeyValueList sortedKeyValueList) =
    SortedKeyValueList
        (sortedKeyValueList
            |> List.map
                (\entry ->
                    { key = entry.key, value = elementChange entry }
                )
        )


{-| The "msg" in a [`Web.program`](#program)
-}
type ProgramEvent appState
    = JsEventFailedToDecode Json.Decode.Error
    | JsEventCouldNotBeAssociated
    | JsEventEnabledConstructionOfNewAppState appState


namespacedKeyToComparable : { namespace : String, key : String } -> String
namespacedKeyToComparable namespacedKey =
    namespacedKey.key ++ String.cons ' ' namespacedKey.namespace


{-| Alternative to `Dict` optimized for fast merge and fast creation.
Would be a terrible fit if we needed fast insert and get.
-}
type SortedKeyValueList key value
    = SortedKeyValueList (List { key : key, value : value })


sortedKeyValueListToList : SortedKeyValueList key value -> List { key : key, value : value }
sortedKeyValueListToList (SortedKeyValueList sortedKeyValueList) =
    sortedKeyValueList


idAndDiffToJson : String -> InterfaceSingleDiff future_ -> Json.Encode.Value
idAndDiffToJson id diff =
    Json.Encode.object
        [ ( "id", id |> Json.Encode.string )
        , ( "diff", diff |> interfaceSingleDiffToJson )
        ]


interfaceSingleDiffToJson : InterfaceSingleDiff future_ -> Json.Encode.Value
interfaceSingleDiffToJson diff =
    case diff of
        Add interfaceSingleInfo ->
            Json.Encode.LocalExtra.variant { tag = "Add", value = interfaceSingleInfo |> interfaceSingleToJson }

        Edit edit ->
            Json.Encode.LocalExtra.variant { tag = "Edit", value = edit |> interfaceSingleEditToJson }

        Remove () ->
            interfaceSingleRemoveJson


interfaceSingleRemoveJson : Json.Encode.Value
interfaceSingleRemoveJson =
    Json.Encode.LocalExtra.variant { tag = "Remove", value = Json.Encode.null }


interfaceSingleEditToJson : InterfaceSingleEdit -> Json.Encode.Value
interfaceSingleEditToJson edit =
    Json.Encode.LocalExtra.variant
        (case edit of
            EditDom editDomDiffsAtPaths ->
                { tag = "EditDom"
                , value =
                    editDomDiffsAtPaths
                        |> Json.Encode.list
                            (\editDomDiffAtPath ->
                                Json.Encode.object
                                    [ ( "path", editDomDiffAtPath.path |> Json.Encode.list Json.Encode.int )
                                    , ( "edit", editDomDiffAtPath.edit |> domEditSingleToJson )
                                    ]
                            )
                }

            EditAudio audioEdits ->
                { tag = "EditAudio"
                , value =
                    Json.Encode.object
                        [ ( "url", audioEdits.url |> Json.Encode.string )
                        , ( "startTime", audioEdits.startTime |> Time.posixToMillis |> Json.Encode.int )
                        , ( "edits"
                          , audioEdits.edits
                                |> Json.Encode.list
                                    (\editDomDiffAtPath ->
                                        Json.Encode.LocalExtra.variant
                                            (case editDomDiffAtPath of
                                                AudioEditSetSpeed new ->
                                                    { tag = "Speed", value = new |> audioParameterTimelineToJson }

                                                AudioEditSetVolume new ->
                                                    { tag = "Volume", value = new |> audioParameterTimelineToJson }

                                                AudioEditSetStereoPan new ->
                                                    { tag = "StereoPan", value = new |> audioParameterTimelineToJson }

                                                AudioEditSetProcessing new ->
                                                    { tag = "Processing"
                                                    , value = new |> Json.Encode.list audioProcessingToJson
                                                    }
                                            )
                                    )
                          )
                        ]
                }

            EditNotification editNotificationDiff ->
                { tag = "EditNotification"
                , value =
                    Json.Encode.object
                        [ ( "id", editNotificationDiff.id |> Json.Encode.string )
                        , ( "message", editNotificationDiff.message |> Json.Encode.string )
                        , ( "details", editNotificationDiff.details |> Json.Encode.string )
                        ]
                }
        )


domEditSingleToJson : DomEditSingle -> Json.Encode.Value
domEditSingleToJson domEditSingle =
    Json.Encode.LocalExtra.variant
        (case domEditSingle of
            DomEditReplaceNode node ->
                { tag = "ReplaceNode", value = node |> domNodeToJson }

            DomEditRemoveLastNSubs removeFromTheLastSubCount ->
                { tag = "RemoveLastNSubs"
                , value = removeFromTheLastSubCount |> Json.Encode.int
                }

            DomEditAppendSubs subNodesToAdd ->
                { tag = "AppendSubs"
                , value = subNodesToAdd |> Json.Encode.list domNodeToJson
                }

            DomEditSetStyles styles ->
                { tag = "SetStyles"
                , value =
                    Json.Encode.object
                        [ ( "remove", styles.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , styles.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                }

            DomEditSetAttributes attributes ->
                { tag = "SetAttributes"
                , value =
                    Json.Encode.object
                        [ ( "remove", attributes.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , attributes.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                }

            DomEditSetAttributesNamespaced attributesNamespaced ->
                { tag = "SetAttributesNamespaced"
                , value =
                    Json.Encode.object
                        [ ( "remove"
                          , attributesNamespaced.remove
                                |> Json.Encode.list
                                    (\removeSingle ->
                                        Json.Encode.object
                                            [ ( "namespace", removeSingle.namespace |> Json.Encode.string )
                                            , ( "key", removeSingle.key |> Json.Encode.string )
                                            ]
                                    )
                          )
                        , ( "edit"
                          , attributesNamespaced.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "namespace", editSingle.namespace |> Json.Encode.string )
                                            , ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                }

            DomEditSetStringProperties stringProperties ->
                { tag = "SetStringProperties"
                , value =
                    Json.Encode.object
                        [ ( "remove", stringProperties.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , stringProperties.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                }

            DomEditSetBoolProperties boolProperties ->
                { tag = "SetBoolProperties"
                , value =
                    Json.Encode.object
                        [ ( "remove", boolProperties.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , boolProperties.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.bool )
                                            ]
                                    )
                          )
                        ]
                }

            DomEditSetScrollToPosition maybePosition ->
                { tag = "SetScrollToPosition"
                , value = maybePosition |> Json.Encode.LocalExtra.nullable domElementScrollPositionToJson
                }

            DomEditSetScrollToShow alignment ->
                { tag = "SetScrollToShow"
                , value = alignment |> Json.Encode.LocalExtra.nullable domElementVisibilityAlignmentsToJson
                }

            DomEditRequestScrollPosition () ->
                { tag = "RequestScrollPosition", value = Json.Encode.null }

            DomEditSetEventListens listens ->
                { tag = "SetEventListens"
                , value =
                    listens
                        |> Json.Encode.list
                            (\entry ->
                                Json.Encode.object
                                    [ ( "name", entry.key |> Json.Encode.string )
                                    , ( "defaultActionHandling", entry.value |> defaultActionHandlingToJson )
                                    ]
                            )
                }
        )


interfaceSingleToJson : InterfaceSingle future_ -> Json.Encode.Value
interfaceSingleToJson interfaceSingle =
    Json.Encode.LocalExtra.variant
        (case interfaceSingle of
            DocumentTitleReplaceBy replacement ->
                { tag = "DocumentTitleReplaceBy", value = replacement |> Json.Encode.string }

            DocumentAuthorSet new ->
                { tag = "DocumentAuthorSet", value = new |> Json.Encode.string }

            DocumentKeywordsSet new ->
                { tag = "DocumentKeywordsSet", value = new |> String.join "," |> Json.Encode.string }

            DocumentDescriptionSet new ->
                { tag = "DocumentDescriptionSet", value = new |> Json.Encode.string }

            ConsoleLog string ->
                { tag = "ConsoleLog", value = string |> Json.Encode.string }

            ConsoleWarn string ->
                { tag = "ConsoleWarn", value = string |> Json.Encode.string }

            ConsoleError string ->
                { tag = "ConsoleError", value = string |> Json.Encode.string }

            NavigationPushUrl url ->
                { tag = "NavigationPushUrl", value = url |> AppUrl.toString |> Json.Encode.string }

            NavigationReplaceUrl url ->
                { tag = "NavigationReplaceUrl", value = url |> AppUrl.toString |> Json.Encode.string }

            NavigationGo urlSteps ->
                { tag = "NavigationGo", value = urlSteps |> Json.Encode.int }

            NavigationLoad url ->
                { tag = "NavigationLoad", value = url |> Json.Encode.string }

            NavigationReload () ->
                { tag = "NavigationReload", value = Json.Encode.null }

            FileDownload config ->
                { tag = "FileDownload"
                , value =
                    Json.Encode.object
                        [ ( "name", config.name |> Json.Encode.string )
                        , ( "mimeType", config.mimeType |> Json.Encode.string )
                        , ( "contentAsciiString"
                          , config.contentAsciiString |> Json.Encode.string
                          )
                        ]
                }

            ClipboardReplaceBy replacement ->
                { tag = "ClipboardReplaceBy"
                , value = replacement |> Json.Encode.string
                }

            AudioPlay audio ->
                { tag = "AudioPlay", value = audio |> audioToJson }

            SocketListen listen ->
                { tag = "SocketListen"
                , value =
                    Json.Encode.object
                        [ ( "address", listen.address |> Json.Encode.string ) ]
                }

            SocketDataSend send ->
                { tag = "SocketDataSend"
                , value =
                    Json.Encode.object
                        [ ( "address", send.address |> Json.Encode.string )
                        , ( "data", send.data |> Json.Encode.string )
                        ]
                }

            LocalStorageSet set ->
                { tag = "LocalStorageSet"
                , value =
                    Json.Encode.object
                        [ ( "key", set.key |> Json.Encode.string )
                        , ( "value", set.value |> Json.Encode.LocalExtra.nullable Json.Encode.string )
                        ]
                }

            NotificationAskForPermission () ->
                { tag = "NotificationAskForPermission", value = Json.Encode.null }

            DomNodeRender domNode ->
                { tag = "DomNodeRender"
                , value = domNode |> domNodeToJson
                }

            AudioSourceLoad sourceLoad ->
                { tag = "AudioSourceLoad", value = sourceLoad.url |> Json.Encode.string }

            NotificationShow show ->
                { tag = "NotificationShow"
                , value =
                    Json.Encode.object
                        [ ( "id", show.id |> Json.Encode.string )
                        , ( "message", show.message |> Json.Encode.string )
                        , ( "details", show.details |> Json.Encode.string )
                        ]
                }

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

            WindowSizeRequest _ ->
                { tag = "WindowSizeRequest", value = Json.Encode.null }

            WindowPreferredLanguagesRequest _ ->
                { tag = "WindowPreferredLanguagesRequest", value = Json.Encode.null }

            NavigationUrlRequest _ ->
                { tag = "NavigationUrlRequest", value = Json.Encode.null }

            ClipboardRequest _ ->
                { tag = "ClipboardRequest", value = Json.Encode.null }

            LocalStorageRequest request ->
                { tag = "LocalStorageRequest"
                , value = Json.Encode.object [ ( "key", request.key |> Json.Encode.string ) ]
                }

            GeoLocationRequest _ ->
                { tag = "GeoLocationRequest", value = Json.Encode.null }

            GamepadsRequest _ ->
                { tag = "GamepadsRequest", value = Json.Encode.null }

            TimePeriodicallyListen intervalDuration ->
                { tag = "TimePeriodicallyListen"
                , value =
                    Json.Encode.object
                        [ ( "milliSeconds", intervalDuration.intervalDurationMilliSeconds |> Json.Encode.int ) ]
                }

            WindowEventListen listen ->
                { tag = "WindowEventListen", value = listen.eventName |> Json.Encode.string }

            WindowVisibilityChangeListen _ ->
                { tag = "WindowVisibilityChangeListen", value = Json.Encode.null }

            WindowAnimationFrameListen _ ->
                { tag = "WindowAnimationFrameListen", value = Json.Encode.null }

            WindowPreferredLanguagesChangeListen _ ->
                { tag = "WindowPreferredLanguagesChangeListen", value = Json.Encode.null }

            MediaQueryRequest request ->
                { tag = "MediaQueryRequest", value = request.queryString |> Json.Encode.string }

            MediaQueryChangeListen listen ->
                { tag = "MediaQueryChangeListen", value = listen.queryString |> Json.Encode.string }

            DocumentEventListen listen ->
                { tag = "DocumentEventListen", value = listen.eventName |> Json.Encode.string }

            LocalStorageRemoveOnADifferentTabListen listen ->
                { tag = "LocalStorageRemoveOnADifferentTabListen"
                , value =
                    Json.Encode.object
                        [ ( "key", listen.key |> Json.Encode.string ) ]
                }

            LocalStorageSetOnADifferentTabListen listen ->
                { tag = "LocalStorageSetOnADifferentTabListen"
                , value =
                    Json.Encode.object
                        [ ( "key", listen.key |> Json.Encode.string ) ]
                }

            GeoLocationChangeListen _ ->
                { tag = "GeoLocationChangeListen", value = Json.Encode.null }

            GamepadsChangeListen _ ->
                { tag = "GamepadsChangeListen", value = Json.Encode.null }
        )


audioToJson : Audio -> Json.Encode.Value
audioToJson audio =
    Json.Encode.object
        [ ( "url", audio.url |> Json.Encode.string )
        , ( "startTime", audio.startTime |> Time.posixToMillis |> Json.Encode.int )
        , ( "volume", audio.volume |> audioParameterTimelineToJson )
        , ( "speed", audio.speed |> audioParameterTimelineToJson )
        , ( "stereoPan", audio.stereoPan |> audioParameterTimelineToJson )
        , ( "processing"
          , audio.processingLastToFirst
                |> List.reverse
                |> Json.Encode.list audioProcessingToJson
          )
        ]


audioParameterTimelineToJson : AudioParameterTimeline -> Json.Encode.Value
audioParameterTimelineToJson timeline =
    Json.Encode.object
        [ ( "startValue", timeline.startValue |> Json.Encode.float )
        , ( "keyFrames"
          , timeline.keyFrames
                |> List.sortBy (\keyFrame -> keyFrame.time |> Time.posixToMillis)
                |> Json.Encode.list
                    (\keyFrame ->
                        Json.Encode.object
                            [ ( "time", keyFrame.time |> Time.posixToMillis |> Json.Encode.int )
                            , ( "value", keyFrame.value |> Json.Encode.float )
                            ]
                    )
          )
        ]


audioProcessingToJson : AudioProcessing -> Json.Encode.Value
audioProcessingToJson processing =
    Json.Encode.LocalExtra.variant
        (case processing of
            AudioLinearConvolution linearConvolution ->
                { tag = "LinearConvolution"
                , value = Json.Encode.object [ ( "sourceUrl", linearConvolution.sourceUrl |> Json.Encode.string ) ]
                }

            AudioLowpass lowpass ->
                { tag = "Lowpass"
                , value = Json.Encode.object [ ( "cutoffFrequency", lowpass.cutoffFrequency |> audioParameterTimelineToJson ) ]
                }

            AudioHighpass highpass ->
                { tag = "highpasses"
                , value = Json.Encode.object [ ( "cutoffFrequency", highpass.cutoffFrequency |> audioParameterTimelineToJson ) ]
                }
        )


domNodeToJson : DomNode future_ -> Json.Encode.Value
domNodeToJson domNode =
    -- IGNORE TCO
    Json.Encode.LocalExtra.variant
        (case domNode of
            DomText text ->
                { tag = "Text", value = text |> Json.Encode.string }

            DomElement element ->
                { tag = "Element"
                , value =
                    Json.Encode.object
                        [ ( "header", element.header |> domElementHeaderInfoToJson )
                        , ( "subs", element.subs |> Json.Encode.list domNodeToJson )
                        ]
                }
        )


domElementHeaderInfoToJson : DomElementHeader future_ -> Json.Encode.Value
domElementHeaderInfoToJson header =
    Json.Encode.object
        [ ( "namespace", header.namespace |> Json.Encode.LocalExtra.nullable Json.Encode.string )
        , ( "tag", header.tag |> Json.Encode.string )
        , ( "styles", header.styles |> domElementStylesToJson )
        , ( "attributes", header.attributes |> domElementAttributesToJson )
        , ( "attributesNamespaced", header.attributesNamespaced |> domElementAttributesNamespacedToJson )
        , ( "stringProperties", header.stringProperties |> domElementStringPropertiesToJson )
        , ( "boolProperties", header.boolProperties |> domElementBoolPropertiesToJson )
        , ( "scrollToPosition"
          , header.scrollToPosition |> Json.Encode.LocalExtra.nullable domElementScrollPositionToJson
          )
        , ( "scrollToShow"
          , header.scrollToShow |> Json.Encode.LocalExtra.nullable domElementVisibilityAlignmentsToJson
          )
        , ( "scrollPositionRequest"
          , Json.Encode.bool
                (case header.scrollPositionRequest of
                    Nothing ->
                        False

                    Just _ ->
                        True
                )
          )
        , ( "eventListens"
          , header.eventListens
                |> sortedKeyValueListToList
                |> Json.Encode.list
                    (\entry ->
                        Json.Encode.object
                            [ ( "name", entry.key |> Json.Encode.string )
                            , ( "defaultActionHandling", entry.value.defaultActionHandling |> defaultActionHandlingToJson )
                            ]
                    )
          )
        ]


domElementAttributesNamespacedToJson :
    SortedKeyValueList
        { namespace : String, key : String }
        String
    -> Json.Encode.Value
domElementAttributesNamespacedToJson attributes =
    attributes
        |> sortedKeyValueListToList
        |> Json.Encode.list
            (\entry ->
                Json.Encode.object
                    [ ( "namespace", entry.key.namespace |> Json.Encode.string )
                    , ( "key", entry.key.key |> Json.Encode.string )
                    , ( "value", entry.value |> Json.Encode.string )
                    ]
            )


domElementAttributesToJson : SortedKeyValueList String String -> Json.Encode.Value
domElementAttributesToJson attributes =
    attributes
        |> sortedKeyValueListToList
        |> Json.Encode.list
            (\entry ->
                Json.Encode.object
                    [ ( "key", entry.key |> Json.Encode.string )
                    , ( "value", entry.value |> Json.Encode.string )
                    ]
            )


domElementStylesToJson : SortedKeyValueList String String -> Json.Encode.Value
domElementStylesToJson styles =
    styles
        |> sortedKeyValueListToList
        |> Json.Encode.list
            (\entry ->
                Json.Encode.object
                    [ ( "key", entry.key |> Json.Encode.string )
                    , ( "value", entry.value |> Json.Encode.string )
                    ]
            )


domElementBoolPropertiesToJson : SortedKeyValueList String Bool -> Json.Encode.Value
domElementBoolPropertiesToJson boolProperties =
    boolProperties
        |> sortedKeyValueListToList
        |> Json.Encode.list
            (\entry ->
                Json.Encode.object
                    [ ( "key", entry.key |> Json.Encode.string )
                    , ( "value", entry.value |> Json.Encode.bool )
                    ]
            )


domElementStringPropertiesToJson : SortedKeyValueList String String -> Json.Encode.Value
domElementStringPropertiesToJson stringProperties =
    stringProperties
        |> sortedKeyValueListToList
        |> Json.Encode.list
            (\entry ->
                Json.Encode.object
                    [ ( "key", entry.key |> Json.Encode.string )
                    , ( "value", entry.value |> Json.Encode.string )
                    ]
            )


defaultActionHandlingToJson : DefaultActionHandling -> Json.Encode.Value
defaultActionHandlingToJson defaultActionHandling =
    Json.Encode.string
        (case defaultActionHandling of
            DefaultActionPrevent ->
                "DefaultActionPrevent"

            DefaultActionExecute ->
                "DefaultActionExecute"
        )


domElementVisibilityAlignmentsToJson : { y : DomElementVisibilityAlignment, x : DomElementVisibilityAlignment } -> Json.Encode.Value
domElementVisibilityAlignmentsToJson alignments =
    Json.Encode.object
        [ ( "x", alignments.x |> domElementVisibilityAlignmentToJson )
        , ( "y", alignments.y |> domElementVisibilityAlignmentToJson )
        ]


domElementVisibilityAlignmentToJson : DomElementVisibilityAlignment -> Json.Encode.Value
domElementVisibilityAlignmentToJson alignment =
    Json.Encode.string
        (case alignment of
            DomElementStart ->
                "start"

            DomElementEnd ->
                "end"

            DomElementCenter ->
                "center"
        )


domElementScrollPositionToJson : { fromLeft : Float, fromTop : Float } -> Json.Encode.Value
domElementScrollPositionToJson position =
    Json.Encode.object
        [ ( "fromLeft", position.fromLeft |> Json.Encode.float )
        , ( "fromTop", position.fromTop |> Json.Encode.float )
        ]


interfaceSingleToStructuredId : InterfaceSingle future_ -> StructuredId
interfaceSingleToStructuredId interfaceSingle =
    StructuredId.ofVariant
        (case interfaceSingle of
            DocumentTitleReplaceBy title ->
                { tag = "DocumentTitleReplaceBy"
                , value = title |> StructuredId.ofString
                }

            DocumentAuthorSet author ->
                { tag = "DocumentAuthorSet"
                , value = author |> StructuredId.ofString
                }

            DocumentKeywordsSet keywords ->
                { tag = "DocumentKeywordsSet"
                , value = keywords |> StructuredId.ofList StructuredId.ofString
                }

            DocumentDescriptionSet description ->
                { tag = "DocumentDescriptionSet"
                , value = description |> StructuredId.ofString
                }

            ConsoleLog message ->
                { tag = "ConsoleLog"
                , value = message |> StructuredId.ofString
                }

            ConsoleWarn message ->
                { tag = "ConsoleWarn"
                , value = message |> StructuredId.ofString
                }

            ConsoleError message ->
                { tag = "ConsoleError"
                , value = message |> StructuredId.ofString
                }

            NavigationReplaceUrl appUrl ->
                { tag = "NavigationReplaceUrl"
                , value = appUrl |> appUrlToStructuredId
                }

            NavigationPushUrl appUrl ->
                { tag = "NavigationPushUrl"
                , value = appUrl |> appUrlToStructuredId
                }

            NavigationGo urlSteps ->
                { tag = "NavigationGo"
                , value = urlSteps |> StructuredId.ofInt
                }

            NavigationLoad url ->
                { tag = "NavigationLoad"
                , value = url |> StructuredId.ofString
                }

            NavigationReload () ->
                { tag = "NavigationReload", value = StructuredId.ofUnit }

            FileDownload config ->
                { tag = "FileDownload"
                , value =
                    StructuredId.ofParts
                        [ config.name |> StructuredId.ofString
                        , config.mimeType |> StructuredId.ofString
                        ]
                }

            ClipboardReplaceBy content ->
                { tag = "ClipboardReplaceBy"
                , value = content |> StructuredId.ofString
                }

            AudioPlay audio ->
                { tag = "AudioPlay"
                , value =
                    StructuredId.ofParts
                        [ audio.url |> StructuredId.ofString
                        , audio.startTime |> Time.LocalExtra.posixToStructureId
                        ]
                }

            SocketListen listen ->
                { tag = "SocketListen"
                , value = listen.address |> StructuredId.ofString
                }

            SocketDataSend message ->
                { tag = "SocketDataSend"
                , value =
                    StructuredId.ofParts
                        [ message.address |> StructuredId.ofString
                        , message.data |> StructuredId.ofString
                        ]
                }

            LocalStorageSet set ->
                { tag = "LocalStorageSet"
                , value =
                    StructuredId.ofParts
                        [ set.key |> StructuredId.ofString
                        , set.value |> StructuredId.ofMaybe StructuredId.ofString
                        ]
                }

            NotificationAskForPermission () ->
                { tag = "NotificationAskForPermission", value = StructuredId.ofUnit }

            DomNodeRender _ ->
                { tag = "DomNodeRender"
                , value = StructuredId.ofUnit
                }

            AudioSourceLoad sourceLoad ->
                { tag = "AudioSourceLoad"
                , value = sourceLoad.url |> StructuredId.ofString
                }

            NotificationShow show ->
                { tag = "NotificationShow"
                , value = show.id |> StructuredId.ofString
                }

            HttpRequestSend send ->
                { tag = "HttpRequestSend"
                , value = send.url |> StructuredId.ofString
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

            LocalStorageRequest request ->
                { tag = "LocalStorageRequest"
                , value = request.key |> StructuredId.ofString
                }

            WindowSizeRequest _ ->
                { tag = "WindowSizeRequest", value = StructuredId.ofUnit }

            WindowPreferredLanguagesRequest _ ->
                { tag = "WindowPreferredLanguagesRequest", value = StructuredId.ofUnit }

            MediaQueryRequest request ->
                { tag = "MediaQueryRequest"
                , value = request.queryString |> StructuredId.ofString
                }

            MediaQueryChangeListen listen ->
                { tag = "MediaQueryChangeListen"
                , value = listen.queryString |> StructuredId.ofString
                }

            NavigationUrlRequest _ ->
                { tag = "NavigationUrlRequest", value = StructuredId.ofUnit }

            ClipboardRequest _ ->
                { tag = "ClipboardRequest", value = StructuredId.ofUnit }

            GeoLocationRequest _ ->
                { tag = "GeoLocationRequest", value = StructuredId.ofUnit }

            GamepadsRequest _ ->
                { tag = "GamepadsRequest", value = StructuredId.ofUnit }

            WindowEventListen listen ->
                { tag = "WindowEventListen"
                , value = listen.eventName |> StructuredId.ofString
                }

            WindowVisibilityChangeListen _ ->
                { tag = "WindowVisibilityChangeListen", value = StructuredId.ofUnit }

            WindowAnimationFrameListen _ ->
                { tag = "WindowAnimationFrameListen", value = StructuredId.ofUnit }

            WindowPreferredLanguagesChangeListen _ ->
                { tag = "WindowPreferredLanguagesChangeListen", value = StructuredId.ofUnit }

            DocumentEventListen listen ->
                { tag = "DocumentEventListen"
                , value = listen.eventName |> StructuredId.ofString
                }

            TimePeriodicallyListen listen ->
                { tag = "TimePeriodicallyListen"
                , value = listen.intervalDurationMilliSeconds |> StructuredId.ofInt
                }

            LocalStorageRemoveOnADifferentTabListen listen ->
                { tag = "LocalStorageRemoveOnADifferentTabListen"
                , value = listen.key |> StructuredId.ofString
                }

            LocalStorageSetOnADifferentTabListen listen ->
                { tag = "LocalStorageSetOnADifferentTabListen"
                , value = listen.key |> StructuredId.ofString
                }

            GeoLocationChangeListen _ ->
                { tag = "GeoLocationChangeListen", value = StructuredId.ofUnit }

            GamepadsChangeListen _ ->
                { tag = "GamepadsChangeListen", value = StructuredId.ofUnit }
        )


appUrlToStructuredId : AppUrl -> StructuredId
appUrlToStructuredId appUrl =
    StructuredId.ofParts
        [ appUrl.path |> StructuredId.ofList StructuredId.ofString
        , appUrl.queryParameters
            |> Dict.toList
            |> StructuredId.ofList
                (\( key, value ) ->
                    StructuredId.ofParts
                        [ key |> StructuredId.ofString
                        , value |> StructuredId.ofList StructuredId.ofString
                        ]
                )
        , appUrl.fragment |> StructuredId.ofMaybe StructuredId.ofString
        ]


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
            let
                newStateJsonDecoder : Json.Decode.Decoder (NotAssociatedOr state)
                newStateJsonDecoder =
                    jsonDecodeFieldIdString
                        |> Json.Decode.andThen
                            (\originalInterfaceId ->
                                case state.interface |> FastDict.get originalInterfaceId of
                                    Nothing ->
                                        jsonDecodeSucceedNotAssociated

                                    Just interfaceSingleAcceptingFuture ->
                                        Json.Decode.field "eventData"
                                            (interfaceSingleAcceptingFuture |> interfaceSingleFutureJsonDecoder)
                            )
            in
            case interfaceJson |> Json.Decode.decodeValue newStateJsonDecoder of
                Ok result ->
                    case result of
                        Associated associated ->
                            JsEventEnabledConstructionOfNewAppState associated

                        NotAssociated ->
                            JsEventCouldNotBeAssociated

                Err error ->
                    JsEventFailedToDecode error
        )


jsonDecodeFieldIdString : Json.Decode.Decoder String
jsonDecodeFieldIdString =
    Json.Decode.field "id" Json.Decode.string


type NotAssociatedOr associated
    = Associated associated
    | NotAssociated


jsonDecodeSucceedNotAssociated : Json.Decode.Decoder (NotAssociatedOr associated_)
jsonDecodeSucceedNotAssociated =
    Json.Decode.succeed NotAssociated


{-| [json `Decoder`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Decoder)
for the transformed event data coming back
-}
interfaceSingleFutureJsonDecoder : InterfaceSingle future -> Json.Decode.Decoder (NotAssociatedOr future)
interfaceSingleFutureJsonDecoder interface =
    case interface of
        DocumentTitleReplaceBy _ ->
            jsonDecodeSucceedNotAssociated

        DocumentAuthorSet _ ->
            jsonDecodeSucceedNotAssociated

        DocumentKeywordsSet _ ->
            jsonDecodeSucceedNotAssociated

        DocumentDescriptionSet _ ->
            jsonDecodeSucceedNotAssociated

        ConsoleLog _ ->
            jsonDecodeSucceedNotAssociated

        ConsoleWarn _ ->
            jsonDecodeSucceedNotAssociated

        ConsoleError _ ->
            jsonDecodeSucceedNotAssociated

        DocumentEventListen listen ->
            listen.on |> Json.Decode.map Associated

        NavigationReplaceUrl _ ->
            jsonDecodeSucceedNotAssociated

        NavigationPushUrl _ ->
            jsonDecodeSucceedNotAssociated

        NavigationGo _ ->
            jsonDecodeSucceedNotAssociated

        NavigationLoad _ ->
            jsonDecodeSucceedNotAssociated

        NavigationReload () ->
            jsonDecodeSucceedNotAssociated

        NavigationUrlRequest toFuture ->
            urlJsonDecoder
                |> Json.Decode.map
                    (\url ->
                        Associated (toFuture (url |> AppUrl.fromUrl))
                    )

        FileDownload _ ->
            jsonDecodeSucceedNotAssociated

        ClipboardReplaceBy _ ->
            jsonDecodeSucceedNotAssociated

        ClipboardRequest toFuture ->
            Json.Decode.string
                |> Json.Decode.map
                    (\content -> Associated (toFuture content))

        AudioSourceLoad sourceLoad ->
            Json.Decode.LocalExtra.resultOkErr
                (Json.Decode.map (\duration -> Ok { url = sourceLoad.url, duration = duration })
                    (Json.Decode.field "durationInSeconds"
                        (Json.Decode.map Duration.seconds Json.Decode.float)
                    )
                )
                (Json.Decode.map Err audioSourceLoadErrorJsonDecoder)
                |> Json.Decode.map (\source -> Associated (sourceLoad.on source))

        AudioPlay _ ->
            jsonDecodeSucceedNotAssociated

        DomNodeRender domNode ->
            Json.Decode.LocalExtra.choice
                [ { tag = "EventListen"
                  , value =
                        domEventListenEventJsonDecoder
                            |> Json.Decode.map
                                (\specificEvent ->
                                    case domNode |> domNodeAtPath specificEvent.path of
                                        Nothing ->
                                            NotAssociated

                                        Just domNodeTheEventIsFiredFor ->
                                            case domNodeTheEventIsFiredFor of
                                                DomText _ ->
                                                    NotAssociated

                                                DomElement domElementTheEventIsFiredFor ->
                                                    {- The fact that this can only be implemented linearly might seem shocking.
                                                       In reality, merging and creating a FastDict.Dict that gets thrown away after the next .get is way heavier (that's the theory at least).
                                                    -}
                                                    case
                                                        domElementTheEventIsFiredFor.header.eventListens
                                                            |> sortedKeyValueListGetAtStringKey specificEvent.name
                                                    of
                                                        Nothing ->
                                                            NotAssociated

                                                        Just eventListen ->
                                                            Associated (eventListen.on specificEvent.value)
                                )
                  }
                , { tag = "ScrollPositionRequest"
                  , value =
                        domElementScrollPositionJsonDecoder
                            |> Json.Decode.map
                                (\specificEvent ->
                                    case domNode |> domNodeAtPath specificEvent.path of
                                        Nothing ->
                                            NotAssociated

                                        Just domNodeTheEventIsFiredFor ->
                                            case domNodeTheEventIsFiredFor of
                                                DomText _ ->
                                                    NotAssociated

                                                DomElement domElementTheEventIsFiredFor ->
                                                    case domElementTheEventIsFiredFor.header.scrollPositionRequest of
                                                        Nothing ->
                                                            NotAssociated

                                                        Just request ->
                                                            Associated
                                                                (request
                                                                    { fromLeft = specificEvent.fromLeft
                                                                    , fromTop = specificEvent.fromTop
                                                                    }
                                                                )
                                )
                  }
                ]

        NotificationAskForPermission () ->
            jsonDecodeSucceedNotAssociated

        NotificationShow show ->
            notificationResponseJsonDecoder
                |> Json.Decode.map (\response -> Associated (show.on response))

        HttpRequestSend request ->
            httpRequestSendEventJsonDecoder
                |> Json.Decode.map (\response -> Associated (request.on response))

        TimePosixRequest toFuture ->
            Time.LocalExtra.posixJsonDecoder
                |> Json.Decode.map (\posix -> Associated (toFuture posix))

        TimezoneOffsetRequest toFuture ->
            Json.Decode.int
                |> Json.Decode.map (\offset -> Associated (toFuture offset))

        TimePeriodicallyListen periodicallyListen ->
            Time.LocalExtra.posixJsonDecoder
                |> Json.Decode.map (\posix -> Associated (periodicallyListen.on posix))

        TimeOnce once ->
            Time.LocalExtra.posixJsonDecoder
                |> Json.Decode.map (\posix -> Associated (once.on posix))

        TimezoneNameRequest toFuture ->
            Json.Decode.string
                |> Json.Decode.map (\zone -> Associated (toFuture zone))

        RandomUnsignedInt32sRequest request ->
            Json.Decode.list Json.Decode.int
                |> Json.Decode.map (\randomness -> Associated (request.on randomness))

        WindowSizeRequest toFuture ->
            windowSizeJsonDecoder
                |> Json.Decode.map (\size -> Associated (toFuture size))

        WindowPreferredLanguagesRequest toFuture ->
            Json.Decode.list Json.Decode.string
                |> Json.Decode.map (\languages -> Associated (toFuture languages))

        WindowEventListen listen ->
            listen.on |> Json.Decode.map Associated

        WindowVisibilityChangeListen toFuture ->
            windowVisibilityJsonDecoder
                |> Json.Decode.map (\visibility -> Associated (toFuture visibility))

        WindowAnimationFrameListen toFuture ->
            Time.LocalExtra.posixJsonDecoder
                |> Json.Decode.map (\posix -> Associated (toFuture posix))

        WindowPreferredLanguagesChangeListen toFuture ->
            Json.Decode.list Json.Decode.string
                |> Json.Decode.map (\languages -> Associated (toFuture languages))

        MediaQueryRequest request ->
            Json.Decode.bool
                |> Json.Decode.map (\matched -> Associated (request.on matched))

        MediaQueryChangeListen listen ->
            Json.Decode.bool
                |> Json.Decode.map (\matched -> Associated (listen.on matched))

        SocketListen listen ->
            socketEventJsonDecoder
                |> Json.Decode.map (\event -> Associated (listen.on event))

        SocketDataSend _ ->
            jsonDecodeSucceedNotAssociated

        LocalStorageSet _ ->
            jsonDecodeSucceedNotAssociated

        LocalStorageRequest request ->
            Json.Decode.nullable Json.Decode.string
                |> Json.Decode.map (\value -> Associated (request.on value))

        LocalStorageRemoveOnADifferentTabListen listen ->
            urlJsonDecoder
                |> Json.Decode.map
                    (\url ->
                        Associated (listen.on (url |> AppUrl.fromUrl))
                    )

        LocalStorageSetOnADifferentTabListen listen ->
            localStorageSetOnADifferentTabEventJsonDecoder
                |> Json.Decode.map (\value -> Associated (listen.on value))

        GeoLocationRequest toFuture ->
            geoLocationJsonDecoder
                |> Json.Decode.map
                    (\geoLocation -> Associated (toFuture geoLocation))

        GeoLocationChangeListen toFuture ->
            geoLocationJsonDecoder
                |> Json.Decode.map
                    (\geoLocation -> Associated (toFuture geoLocation))

        GamepadsRequest toFuture ->
            gamepadsJsonDecoder
                |> Json.Decode.map
                    (\gamepads -> Associated (toFuture gamepads))

        GamepadsChangeListen toFuture ->
            gamepadsJsonDecoder
                |> Json.Decode.map
                    (\gamepads -> Associated (toFuture gamepads))


domNodeAtPath : List Int -> DomNode future -> Maybe (DomNode future)
domNodeAtPath path domNode =
    case path of
        [] ->
            Just domNode

        subIndex :: subPath ->
            case domNode of
                DomText _ ->
                    Nothing

                DomElement element ->
                    case element.subs |> List.LocalExtra.atIndex subIndex of
                        Nothing ->
                            Nothing

                        Just subDomNode ->
                            domNodeAtPath subPath subDomNode


urlJsonDecoder : Json.Decode.Decoder Url
urlJsonDecoder =
    Json.Decode.andThen
        (\urlString ->
            case urlString |> Url.fromString of
                Nothing ->
                    Json.Decode.fail "invalid URL"

                Just urlParsed ->
                    Json.Decode.succeed urlParsed
        )
        Json.Decode.string


domEventListenEventJsonDecoder :
    Json.Decode.Decoder
        { name : String
        , path : List Int
        , value : Json.Decode.Value
        }
domEventListenEventJsonDecoder =
    Json.Decode.map3
        (\name path value ->
            { name = name
            , path = path
            , value = value
            }
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "path" (Json.Decode.list Json.Decode.int))
        (Json.Decode.field "event" Json.Decode.value)


httpRequestSendEventJsonDecoder : Json.Decode.Decoder (Result HttpError Bytes)
httpRequestSendEventJsonDecoder =
    Json.Decode.LocalExtra.resultOkErr
        httpSuccessResponseJsonDecoder
        (Json.Decode.map Err httpErrorJsonDecoder)


windowSizeJsonDecoder : Json.Decode.Decoder { width : Int, height : Int }
windowSizeJsonDecoder =
    Json.Decode.map2 (\width height -> { width = width, height = height })
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


localStorageSetOnADifferentTabEventJsonDecoder : Json.Decode.Decoder { appUrl : AppUrl, oldValue : Maybe String, newValue : String }
localStorageSetOnADifferentTabEventJsonDecoder =
    Json.Decode.map3
        (\appUrl oldValue newValue ->
            { appUrl = appUrl, oldValue = oldValue, newValue = newValue }
        )
        (Json.Decode.field "url"
            (urlJsonDecoder |> Json.Decode.map AppUrl.fromUrl)
        )
        (Json.Decode.field "oldValue" (Json.Decode.nullable Json.Decode.string))
        (Json.Decode.field "newValue" Json.Decode.string)


sortedKeyValueListGetAtStringKey : String -> SortedKeyValueList String value -> Maybe value
sortedKeyValueListGetAtStringKey keyToFind sortedKeyValueList =
    sortedKeyValueList
        |> sortedKeyValueListToList
        |> List.LocalExtra.firstJustMap
            (\entry ->
                if entry.key == keyToFind ++ "" then
                    Just entry.value

                else
                    Nothing
            )


audioSourceLoadErrorJsonDecoder : Json.Decode.Decoder AudioSourceLoadError
audioSourceLoadErrorJsonDecoder =
    Json.Decode.string
        |> Json.Decode.map
            (\errorMessage ->
                case errorMessage of
                    "NetworkError" ->
                        AudioSourceLoadNetworkError

                    "MediaDecodeAudioDataUnknownContentType" ->
                        AudioSourceLoadDecodeError

                    "DOMException: The buffer passed to decodeAudioData contains an unknown content type." ->
                        AudioSourceLoadDecodeError

                    _ ->
                        AudioSourceLoadNetworkError
            )


domElementScrollPositionJsonDecoder :
    Json.Decode.Decoder
        { path : List Int
        , fromLeft : Float
        , fromTop : Float
        }
domElementScrollPositionJsonDecoder =
    Json.Decode.map3
        (\path fromLeft fromTop ->
            { path = path
            , fromLeft = fromLeft
            , fromTop = fromTop
            }
        )
        (Json.Decode.field "path" (Json.Decode.list Json.Decode.int))
        (Json.Decode.field "fromLeft" Json.Decode.float)
        (Json.Decode.field "fromTop" Json.Decode.float)


notificationResponseJsonDecoder : Json.Decode.Decoder NotificationClicked
notificationResponseJsonDecoder =
    Json.Decode.map (\() -> NotificationClicked)
        (Json.Decode.LocalExtra.onlyString "Clicked")


socketEventJsonDecoder : Json.Decode.Decoder SocketEvent
socketEventJsonDecoder =
    Json.Decode.LocalExtra.choice
        [ { tag = "SocketOpened"
          , value = Json.Decode.null SocketOpened
          }
        , { tag = "SocketDataReceived"
          , value =
                Json.Decode.map SocketDataReceived
                    Json.Decode.string
          }
        , { tag = "SocketClosed"
          , value =
                Json.Decode.map2 (\code reason -> SocketClosed { code = code, reason = reason })
                    (Json.Decode.field "code" Json.Decode.int)
                    (Json.Decode.field "reason" Json.Decode.string)
          }
        ]


geoLocationJsonDecoder : Json.Decode.Decoder GeoLocation
geoLocationJsonDecoder =
    Json.Decode.map7
        (\latitudeInDecimalDegrees longitudeInDecimalDegrees latitudeLongitudeAccuracy altitudeAboveNominalSeaLevel altitudeAccuracy headingWith0AsTrueNorthAndIncreasingClockwise speed ->
            { latitudeInDecimalDegrees = latitudeInDecimalDegrees
            , longitudeInDecimalDegrees = longitudeInDecimalDegrees
            , latitudeLongitudeAccuracy = latitudeLongitudeAccuracy
            , altitudeAboveNominalSeaLevel = altitudeAboveNominalSeaLevel
            , altitudeAccuracy = altitudeAccuracy
            , headingWith0AsTrueNorthAndIncreasingClockwise = headingWith0AsTrueNorthAndIncreasingClockwise
            , speed = speed
            }
        )
        Json.Decode.float
        Json.Decode.float
        (Json.Decode.nullable (Json.Decode.map Length.meters Json.Decode.float))
        (Json.Decode.nullable (Json.Decode.map Length.meters Json.Decode.float))
        (Json.Decode.nullable (Json.Decode.map Length.meters Json.Decode.float))
        (Json.Decode.map
            (\maybeDegrees ->
                case maybeDegrees of
                    Nothing ->
                        Nothing

                    Just degrees ->
                        if degrees |> Basics.isNaN then
                            Nothing

                        else
                            Angle.degrees degrees |> Just
            )
            (Json.Decode.nullable Json.Decode.float)
        )
        (Json.Decode.nullable (Json.Decode.map Speed.metersPerSecond Json.Decode.float))


gamepadsJsonDecoder : Json.Decode.Decoder (Dict.Dict Int Gamepad)
gamepadsJsonDecoder =
    Json.Decode.map
        (\maybeGamepads ->
            maybeGamepads
                |> List.LocalExtra.foldUpIndexedFrom Dict.empty
                    (\index maybeGamepad soFar ->
                        case maybeGamepad of
                            Nothing ->
                                soFar

                            Just gamepad ->
                                soFar |> Dict.insert index gamepad
                    )
        )
        (Json.Decode.list maybeGamepadJsonDecoder)


maybeGamepadJsonDecoder : Json.Decode.Decoder (Maybe Gamepad)
maybeGamepadJsonDecoder =
    Json.Decode.nullable
        (Json.Decode.field "connected" Json.Decode.bool
            |> Json.Decode.andThen
                (\connected ->
                    if connected then
                        Json.Decode.map Just gamepadJsonDecoder

                    else
                        jsonDecodeSucceedNothing
                )
        )
        |> Json.Decode.map (Maybe.andThen identity)


jsonDecodeSucceedNothing : Json.Decode.Decoder (Maybe a_)
jsonDecodeSucceedNothing =
    Json.Decode.succeed Nothing


gamepadJsonDecoder : Json.Decode.Decoder Gamepad
gamepadJsonDecoder =
    Json.Decode.map3
        (\kindId buttons thumbsticks ->
            let
                buttonMap : GamepadButtonMap
                buttonMap =
                    buttonMapping |> FastDict.get kindId |> Maybe.withDefault gamepadStandardButtonMap

                at : (GamepadButtonMap -> Maybe Int) -> GamepadButton
                at indexField =
                    case buttonMap |> indexField of
                        Nothing ->
                            gamepadButtonUnknown

                        Just index ->
                            buttons |> List.LocalExtra.atIndex index |> Maybe.withDefault gamepadButtonUnknown
            in
            { primaryButton = at .primary
            , secondaryButton = at .secondary
            , tertiaryButton = at .tertiary
            , quaternaryButton = at .quaternary
            , leftBumperButton = at .leftBumper
            , rightBumperButton = at .rightBumper
            , leftTriggerButton = at .leftTrigger
            , rightTriggerButton = at .rightTrigger
            , selectButton = at .select
            , startButton = at .start
            , leftThumbstickButton = at .leftThumbstickButton
            , rightThumbstickButton = at .rightThumbstickButton
            , upButton = at .arrowUp
            , downButton = at .arrowDown
            , leftButton = at .arrowLeft
            , rightButton = at .arrowRight
            , homeButton = at .homeButton
            , touchpadButton = at .touchpad
            , additionalButtons =
                let
                    mappedButtonIndexes : List Int
                    mappedButtonIndexes =
                        [ .primary, .secondary, .tertiary, .quaternary, .leftBumper, .rightBumper, .leftTrigger, .rightTrigger, .select, .start, .leftThumbstickButton, .rightThumbstickButton, .arrowUp, .arrowDown, .arrowLeft, .arrowRight, .homeButton, .touchpad ]
                            |> List.filterMap (\field -> buttonMap |> field)
                in
                buttons
                    |> List.LocalExtra.justsMapIndexed
                        (\index button ->
                            if mappedButtonIndexes |> List.member index then
                                Nothing

                            else
                                button |> Just
                        )
            , kindId = kindId
            , thumbstickLeft = thumbsticks.left
            , thumbstickRight = thumbsticks.right
            , thumbsticksAdditional = thumbsticks.additional
            }
        )
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "buttons" (Json.Decode.list gamepadButtonJsonDecoder))
        (Json.Decode.list Json.Decode.float
            |> Json.Decode.field "axes"
            |> Json.Decode.map
                (\axes ->
                    case (axes |> gamepadThumbsticksFromAxes) |> listPadToAtLeast 2 gamepadThumbstickUnknown of
                        left :: right :: additional ->
                            { left = left
                            , right = right
                            , additional = additional
                            }

                        -- can't happen
                        _ ->
                            { left = gamepadThumbstickUnknown, right = gamepadThumbstickUnknown, additional = [] }
                )
        )


gamepadButtonUnknown : GamepadButton
gamepadButtonUnknown =
    GamepadButtonReleased { isTouched = False }


gamepadThumbstickUnknown : { x : Float, y : Float }
gamepadThumbstickUnknown =
    { x = 0, y = 0 }


gamepadThumbsticksFromAxes : List Float -> List { x : Float, y : Float }
gamepadThumbsticksFromAxes axes =
    -- IGNORE TCO
    -- as axes list will always be small
    case axes of
        x :: y :: rest ->
            { x = x, y = y } :: gamepadThumbsticksFromAxes rest

        _ ->
            []


gamepadButtonJsonDecoder : Json.Decode.Decoder GamepadButton
gamepadButtonJsonDecoder =
    Json.Decode.map3
        (\isPressed isTouched value ->
            if isPressed then
                GamepadButtonPressed { firmnessPercentage = value }

            else
                GamepadButtonReleased { isTouched = isTouched }
        )
        (Json.Decode.field "pressed" Json.Decode.bool)
        (Json.Decode.field "touched" Json.Decode.bool)
        (Json.Decode.field "value" Json.Decode.float)


listPadToAtLeast : Int -> a -> List a -> List a
listPadToAtLeast newMinimumSize paddingElement list =
    list
        ++ List.repeat (newMinimumSize - (list |> List.length)) paddingElement


{-| using

  - <https://dark.elm.dmy.fr/packages/noordstar/elm-gamepad/latest>
  - <https://github.com/bluebitgame/GamepadServer/blob/6124d3d9a6d83f2efca21e53eb944c554b5755b5/web/gamepad.js#L9>
  - <https://github.com/bmsuseluda/emuze/blob/ecc0c35a3b7399142f550d06eba89e88e6ca3f01/app/hooks/useGamepads/tests/gamepadTypeMapping.test.ts#L36>
  - <https://github.com/beetrootpaul/beetpx/blob/2a5c38a0f5fbcd76efcd0d97e42566594c79eae9/src/game_input/gamepad_mapping/GamepadMapping8BitDo.ts>
  - <https://github.com/philschatz/puzzlescript/tree/5f1444f426691fd8e87a817bf5821b6c71742381/packages/puzzlescript-web/src/browser/controller/configs>
  - <https://github.com/bomblebees/bomblebees/tree/417156f942b7bd6bbee48f1f97b3611bf4db70d8/Assets/Plugins/InControl/Source/Unity/DeviceProfiles>
  - <https://github.com/ericlathrop/html5-gamepad/blob/master/mappings>

to consider adding

  - <https://github.com/electrovir/gamepad-type>
  - <https://github.com/STREGAsGate/GateEngine/blob/07bbc90668fafe211830b535128eee75a98aef3f/Sources/GateEngine/System/HID/GamePad/GamePadInterpreter/Interpreters/WASIGamePadInterpreter.swift#L58>
  - <https://github.com/mdqinc/SDL_GameControllerDB>

On many other models I didn't find good examples so I don't feel confident enough giving a mapping

-}
buttonMapping : FastDict.Dict String GamepadButtonMap
buttonMapping =
    FastDict.empty
        |> fastDictInsertSameValueFor
            [ "xinput", "Wireless Controller (STANDARD GAMEPAD)" ]
            gamepadStandardButtonMap
        |> fastDictInsertSameValueFor
            [ "Xbox 360 Controller (XInput STANDARD GAMEPAD)"
            , "Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 05c4)"
            , "Xbox Wireless Controller (STANDARD GAMEPAD Vendor: 045e Product: 02fd)"
            , "HID-compliant game controller (STANDARD GAMEPAD Vendor: 045e Product: 02fd)"
            ]
            { gamepadStandardButtonMap | touchpad = Nothing }
        |> fastDictInsertSameValueFor
            [ "Xbox Wireless Controller Extended Gamepad"
            , "Unknown Gamepad (Vendor: beef Product: 046d)"
            ]
            gamepadStandardButtonMap
        |> fastDictInsertSameValueFor
            [ "054c-05c4-Wireless Controller", "Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 05c4)" ]
            gamepadStandardButtonMap
        |> fastDictInsertSameValueFor
            [ "45e-28e-Xbox 360 Wired Controller" ]
            { primary = Just 11
            , secondary = Just 12
            , tertiary = Just 13
            , quaternary = Just 14
            , leftBumper = Just 8
            , rightBumper = Just 9
            , touchpad = Nothing
            , select = Just 5
            , start = Just 4
            , arrowUp = Just 0
            , arrowDown = Just 1
            , arrowLeft = Just 2
            , arrowRight = Just 3
            , leftThumbstickButton = Just 6
            , rightThumbstickButton = Just 7
            , homeButton = Just 10
            , -- these are only analog
              leftTrigger = Nothing
            , rightTrigger = Nothing
            }
        |> fastDictInsertSameValueFor
            [ "0810-0001- USB Gamepad          ", " USB Gamepad           (Vendor: 0810 Product: 0001)" ]
            { gamepadStandardButtonMap
                | primary = Just 2
                , tertiary = Just 3
                , quaternary = Just 0
                , touchpad = Nothing
            }
        |> fastDictInsertSameValueFor
            [ "0810-e501-usb gamepad           ", "usb gamepad            (Vendor: 0810 Product: e501)" ]
            { gamepadStandardButtonMap
                | primary = Just 1
                , secondary = Just 2
                , tertiary = Just 0
                , leftTrigger = Nothing
                , rightTrigger = Nothing
                , leftThumbstickButton = Nothing
                , rightThumbstickButton = Nothing
                , arrowUp = Nothing
                , arrowLeft = Nothing
                , arrowRight = Nothing
                , arrowDown = Nothing
                , homeButton = Nothing
            }
        |> -- nintendo style
           fastDictInsertSameValueFor
            [ "057e-2009-Pro Controller"
            , "Pro Controller (STANDARD GAMEPAD Vendor: 057e Product: 2009)"
            , "057e-2009-Pro Wireless Gamepad"
            , "Wireless Gamepad (STANDARD GAMEPAD Vendor: 057e Product: 2009)"
            ]
            { gamepadStandardButtonMap
                | primary = Just 1
                , secondary = Just 0
                , quaternary = Just 2
            }
        |> fastDictInsertSameValueFor
            [ "Pro Controller Extended Gamepad" ]
            { gamepadStandardButtonMap
                | primary = Just 1
                , secondary = Just 0
                , quaternary = Just 2
            }
        |> -- dualsense style
           -- too unclear to me how arrows and dpad are interpreted.
           -- Like, some different button indexes belong to the same button output.
           -- And some button values seem to be mixed up in the axis values
           --     [ "54c-ce6-DualSense Wireless Controller"
           --     , "DualSense Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 0ce6)"
           --     , "054c-0ce6-Wireless Controller"
           --     , "Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 0ce6)"
           --     ]
           -- 8BitDo style
           -- too unclear to me how arrows and dpad are interpreted. Some button values seem to be mixed up in the axis values
           --    [ "2dc8-5112-8BitDo Lite 2"
           --    , "8BitDo Lite 2 (Vendor: 2dc8 Product: 5112)"
           --    , "2dc8-5112-Bluetooth Wireless Controller   "
           --    , "Bluetooth Wireless Controller    (Vendor: 2dc8 Product: 5112)"
           --    , "8BitDo Lite 2 Extended Gamepad"
           --    ]
           --
           -- playstation 3 style
           fastDictInsertSameValueFor
            [ "54c-268-PLAYSTATION(R)3 Controller"
            , "PS3 GamePad (Vendor: 054c Product: 0268)"
            ]
            { primary = Just 14
            , secondary = Just 13
            , tertiary = Just 15
            , quaternary = Just 12
            , leftBumper = Just 10
            , rightBumper = Just 11
            , leftTrigger = Just 8
            , rightTrigger = Just 9
            , select = Just 0
            , start = Just 3
            , leftThumbstickButton = Just 1
            , rightThumbstickButton = Just 2
            , arrowUp = Just 4
            , arrowDown = Just 6
            , arrowLeft = Just 7
            , arrowRight = Just 5
            , homeButton = Nothing
            , touchpad = Nothing
            }
        |> -- playstation 4 style
           fastDictInsertSameValueFor
            [ "54c-9cc-Wireless Controller" ]
            { primary = Just 1
            , secondary = Just 2
            , tertiary = Just 0
            , quaternary = Just 3
            , leftBumper = Just 4
            , rightBumper = Just 5
            , leftTrigger = Just 6
            , rightTrigger = Just 7
            , select = Just 8
            , start = Just 9
            , leftThumbstickButton = Just 10
            , rightThumbstickButton = Just 11
            , arrowUp = Just 14
            , arrowDown = Just 15
            , arrowLeft = Just 16
            , arrowRight = Just 17
            , homeButton = Just 12
            , touchpad = Just 13
            }
        |> -- logi style
           fastDictInsertSameValueFor
            [ "046d- c216-Logitech Dual Action"
            , "Logitech Dual Action (STANDARD GAMEPAD Vendor: 046d Product: c216)"
            ]
            { primary = Just 0
            , secondary = Just 1
            , tertiary = Just 2
            , quaternary = Just 3
            , leftBumper = Just 4
            , rightBumper = Just 5
            , leftTrigger = Just 6
            , rightTrigger = Just 7
            , select = Just 8
            , start = Just 9
            , leftThumbstickButton = Just 10
            , rightThumbstickButton = Just 11
            , arrowUp = Just 12
            , arrowDown = Just 13
            , arrowLeft = Just 14
            , arrowRight = Just 15
            , homeButton = Nothing
            , touchpad = Nothing
            }


fastDictInsertSameValueFor :
    List comparableKey
    -> value
    -> FastDict.Dict comparableKey value
    -> FastDict.Dict comparableKey value
fastDictInsertSameValueFor keyList value dict =
    keyList
        |> List.foldl
            (\key dictSoFar -> dictSoFar |> FastDict.insert key value)
            dict


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


windowVisibilityJsonDecoder : Json.Decode.Decoder WindowVisibility
windowVisibilityJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> WindowShown) (Json.Decode.LocalExtra.onlyString "visible")
        , Json.Decode.map (\() -> WindowHidden) (Json.Decode.LocalExtra.onlyString "hidden")
        ]


{-| <https://www.w3.org/TR/gamepad/#remapping>
-}
gamepadStandardButtonMap : GamepadButtonMap
gamepadStandardButtonMap =
    { primary = Just 0
    , secondary = Just 1
    , tertiary = Just 2
    , quaternary = Just 3
    , leftBumper = Just 4
    , rightBumper = Just 5
    , leftTrigger = Just 6
    , rightTrigger = Just 7
    , select = Just 8
    , start = Just 9
    , leftThumbstickButton = Just 10
    , rightThumbstickButton = Just 11
    , arrowUp = Just 12
    , arrowDown = Just 13
    , arrowLeft = Just 14
    , arrowRight = Just 15
    , homeButton = Just 16
    , touchpad = Just 17
    }


{-| Controller information on button presses, thumbstick positions etc.

  - `primaryButton`: The most common action like "enter"/"confirm" or jump

  - `secondaryButton`: Usually "cancel"/"skip" or a common ability like duck/sneak, drop or heal

  - `tertiaryButton`: common-ish ability like consume, interact, reload or an ultimate power

  - `quaternaryButton`: less common action like quick menu or mode switch

  - `leftBumperButton`, `rightBumperButton`: The top row of smaller buttons on the side opposite of you, sometimes called shoulder buttons.
    Often used for alternative menu actions like slot switching or quick map

  - `leftTriggerButton`, `rightTriggerButton`: The bottom row of larger buttons on the side opposite of you.
    Often used for holdable abilities like sliding or dashing

  - `selectButton`: Usually opens an in-world menu with for example inventory, lore, ability overview or a map

  - `startButton`: Usually pauses the game and opens a menu with options like settings and quit

  - `leftThumbstickButton`, `rightThumbstickButton`: Not all gamepads have these, so they often either double existing actions like "confirm"
    or perform actions that are only very rarely helpful, like hiding ui elements or making a screenshot

  - `upButton`, `downBottom`, `leftButton`, `rightButton`: exactly one step in a direction, usually in a (quick) menu/inventory

  - `homeButton`: Usually turns the gamepad on/off, or changes the state of the game

  - `touchpadButton`: Not present on most gamepads. While the touchpad is often used for controlling the mouse, it can also be used as a simple button

  - `thumbstickLeft`, `thumbstickRight`: Those wiggly that can be moved in any direction by any amount.
    They are provided as `x, y` signed percentages

  - `kindId`: some information about the gamepad, usually containing the USB vendor, product id of the gamepad
    and the name of the gamepad as provided by the driver. See [mdn](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad/id)

    You can use this information to for example determine how to show controls

  - `buttonsAdditional`, `thumbsticksAdditional`: Maybe you have a weird gamepad with 3 thumbsticks? These might help 🤷

Implementation note:
As you know, gamepad layouts differ between models.
For most of them, we're able to map them to the buttons and thumbsticks above.
If you experience issues with some model, [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)

-}
type alias Gamepad =
    RecordWithoutConstructorFunction
        { primaryButton : GamepadButton
        , secondaryButton : GamepadButton
        , tertiaryButton : GamepadButton
        , quaternaryButton : GamepadButton
        , leftBumperButton : GamepadButton
        , rightBumperButton : GamepadButton
        , leftTriggerButton : GamepadButton
        , rightTriggerButton : GamepadButton
        , selectButton : GamepadButton
        , startButton : GamepadButton
        , leftThumbstickButton : GamepadButton
        , rightThumbstickButton : GamepadButton
        , upButton : GamepadButton
        , downButton : GamepadButton
        , leftButton : GamepadButton
        , rightButton : GamepadButton
        , homeButton : GamepadButton
        , touchpadButton : GamepadButton
        , additionalButtons : List GamepadButton
        , kindId : String
        , thumbstickLeft : { x : Float, y : Float }
        , thumbstickRight : { x : Float, y : Float }
        , thumbsticksAdditional : List { x : Float, y : Float }
        }


{-| Buttons are either held down with an optional value between 0 and 1 to measure how hard,
or they are released with an optional detection of touch which defaults to false.
-}
type GamepadButton
    = GamepadButtonPressed { firmnessPercentage : Float }
    | GamepadButtonReleased { isTouched : Bool }


type alias GamepadButtonMap =
    RecordWithoutConstructorFunction
        { primary : Maybe Int
        , secondary : Maybe Int
        , tertiary : Maybe Int
        , quaternary : Maybe Int
        , start : Maybe Int
        , select : Maybe Int
        , leftThumbstickButton : Maybe Int
        , rightThumbstickButton : Maybe Int
        , arrowUp : Maybe Int
        , arrowLeft : Maybe Int
        , arrowRight : Maybe Int
        , arrowDown : Maybe Int
        , leftTrigger : Maybe Int
        , leftBumper : Maybe Int
        , rightTrigger : Maybe Int
        , rightBumper : Maybe Int
        , homeButton : Maybe Int
        , touchpad : Maybe Int
        }


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
                    ConsoleError
                        ("bug: js event skipped because it failed to decode: "
                            ++ (jsonError |> Json.Decode.errorToString)
                            ++ "\n\nPlease open an issue on github.com/lue-bird/elm-state-interface-experimental"
                        )
              in
              idAndDiffToJson
                (notifyOfSkippedEventInterface |> interfaceSingleToStructuredId |> StructuredId.toString)
                (notifyOfSkippedEventInterface |> Add)
                |> appConfig.ports.toJs
            )

        JsEventCouldNotBeAssociated ->
            -- this usually happens when multiple events are sent simultaneously from js
            -- and the interface changes between the two.
            -- e.g. we listen for mouse move events while dragging and remove the listen
            -- after mouse up. But since mouse move events are frequent, there still might be
            -- a few move events coming through before their listens are removed on the js side.
            --
            -- Historical note, this was once reported as errors (like json decode errors)
            -- but happened barely frequent enough to be annoying while not being helpful.
            ( state, Cmd.none )

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
        (\id old updated soFar ->
            case { old = old, updated = updated } |> interfaceSingleEdit of
                Nothing ->
                    soFar

                Just edit ->
                    idAndDiffCombine id (Edit edit)
                        :: soFar
        )
        (\addedId onlyNew soFar ->
            idAndDiffCombine addedId (Add onlyNew)
                :: soFar
        )
        interfaces.old
        interfaces.updated
        []


interfaceSingleEdit :
    { old : InterfaceSingle future, updated : InterfaceSingle future }
    -> Maybe InterfaceSingleEdit
interfaceSingleEdit interfaces =
    case interfaces.old of
        DomNodeRender domElementPreviouslyRendered ->
            case interfaces.updated of
                DomNodeRender domElementToRender ->
                    EditDom
                        ({ old = domElementPreviouslyRendered, updated = domElementToRender }
                            |> domNodeDiffMap
                                (\diffAtPath ->
                                    { path = diffAtPath.path
                                    , edit = diffAtPath.edit
                                    }
                                )
                            |> Rope.toList
                        )
                        |> Just

                _ ->
                    Nothing

        AudioPlay previouslyPlayed ->
            case interfaces.updated of
                AudioPlay toPlay ->
                    EditAudio
                        { url = toPlay.url
                        , startTime = toPlay.startTime
                        , edits =
                            { old = previouslyPlayed, updated = toPlay }
                                |> audioDiff
                        }
                        |> Just

                _ ->
                    Nothing

        NotificationShow _ ->
            case interfaces.updated of
                NotificationShow toShow ->
                    { id = toShow.id, message = toShow.message, details = toShow.details }
                        |> EditNotification
                        |> Just

                _ ->
                    Nothing

        DocumentTitleReplaceBy _ ->
            Nothing

        DocumentAuthorSet _ ->
            Nothing

        DocumentKeywordsSet _ ->
            Nothing

        DocumentDescriptionSet _ ->
            Nothing

        DocumentEventListen _ ->
            Nothing

        ConsoleLog _ ->
            Nothing

        ConsoleWarn _ ->
            Nothing

        ConsoleError _ ->
            Nothing

        NavigationReplaceUrl _ ->
            Nothing

        NavigationPushUrl _ ->
            Nothing

        NavigationGo _ ->
            Nothing

        NavigationLoad _ ->
            Nothing

        NavigationReload () ->
            Nothing

        NavigationUrlRequest _ ->
            Nothing

        FileDownload _ ->
            Nothing

        ClipboardReplaceBy _ ->
            Nothing

        ClipboardRequest _ ->
            Nothing

        AudioSourceLoad _ ->
            Nothing

        NotificationAskForPermission () ->
            Nothing

        HttpRequestSend _ ->
            Nothing

        TimePosixRequest _ ->
            Nothing

        TimezoneOffsetRequest _ ->
            Nothing

        TimeOnce _ ->
            Nothing

        TimePeriodicallyListen _ ->
            Nothing

        TimezoneNameRequest _ ->
            Nothing

        RandomUnsignedInt32sRequest _ ->
            Nothing

        WindowSizeRequest _ ->
            Nothing

        WindowPreferredLanguagesRequest _ ->
            Nothing

        WindowEventListen _ ->
            Nothing

        WindowVisibilityChangeListen _ ->
            Nothing

        WindowAnimationFrameListen _ ->
            Nothing

        WindowPreferredLanguagesChangeListen _ ->
            Nothing

        MediaQueryRequest _ ->
            Nothing

        MediaQueryChangeListen _ ->
            Nothing

        SocketListen _ ->
            Nothing

        SocketDataSend _ ->
            Nothing

        LocalStorageSet _ ->
            Nothing

        LocalStorageRequest _ ->
            Nothing

        LocalStorageRemoveOnADifferentTabListen _ ->
            Nothing

        LocalStorageSetOnADifferentTabListen _ ->
            Nothing

        GeoLocationRequest _ ->
            Nothing

        GeoLocationChangeListen _ ->
            Nothing

        GamepadsRequest _ ->
            Nothing

        GamepadsChangeListen _ ->
            Nothing


domNodeDiffMap :
    ({ path : List Int, edit : DomEditSingle } -> fromDomEdit)
    -> { old : DomNode future, updated : DomNode future }
    -> Rope fromDomEdit
domNodeDiffMap fromDomEdit nodes =
    domNodeDiffMapFrom Rope.empty
        fromDomEdit
        nodes


domNodeDiffMapFrom :
    Rope fromDomEdit
    -> ({ path : List Int, edit : DomEditSingle } -> fromDomEdit)
    -> { old : DomNode future, updated : DomNode future }
    -> Rope fromDomEdit
domNodeDiffMapFrom soFar fromDomEdit nodes =
    case nodes.old of
        DomText oldText ->
            case nodes.updated of
                DomElement updatedElement ->
                    soFar
                        |> Rope.append
                            ({ path = []
                             , edit =
                                DomEditReplaceNode
                                    (DomElement
                                        (updatedElement
                                            |> domElementFutureMap (\_ -> ())
                                        )
                                    )
                             }
                                |> fromDomEdit
                            )

                DomText updatedText ->
                    if oldText == updatedText ++ "" then
                        soFar

                    else
                        soFar
                            |> Rope.append
                                ({ path = []
                                 , edit = DomEditReplaceNode (DomText updatedText)
                                 }
                                    |> fromDomEdit
                                )

        DomElement oldElement ->
            case nodes.updated of
                DomText textContent ->
                    soFar
                        |> Rope.append
                            ({ path = []
                             , edit = DomEditReplaceNode (DomText textContent)
                             }
                                |> fromDomEdit
                            )

                DomElement updatedElement ->
                    if oldElement.header.tag /= updatedElement.header.tag then
                        soFar
                            |> Rope.append
                                ({ path = []
                                 , edit =
                                    DomEditReplaceNode
                                        (DomElement
                                            (updatedElement
                                                |> domElementFutureMap (\_ -> ())
                                            )
                                        )
                                 }
                                    |> fromDomEdit
                                )

                    else
                        domSubNodesDiffMapAtParentPathFromIndexAndSoFar []
                            0
                            (Rope.appendTo
                                ({ old = oldElement.header, updated = updatedElement.header }
                                    |> domElementHeaderDiffMap
                                        (\edit ->
                                            { path = [], edit = edit }
                                                |> fromDomEdit
                                        )
                                    |> Rope.fromList
                                )
                                soFar
                            )
                            fromDomEdit
                            { old = oldElement.subs
                            , updated = updatedElement.subs
                            }


domSubNodesDiffMapAtParentPathFromIndexAndSoFar :
    List Int
    -> Int
    -> Rope fromDomEdit
    -> ({ path : List Int, edit : DomEditSingle } -> fromDomEdit)
    ->
        { old : List (DomNode future)
        , updated : List (DomNode future)
        }
    -> Rope fromDomEdit
domSubNodesDiffMapAtParentPathFromIndexAndSoFar parentPath index soFar fromDomEdit nodes =
    case nodes.old of
        [] ->
            case nodes.updated of
                [] ->
                    soFar

                updatedSubHead :: updatedSubsTail ->
                    soFar
                        |> Rope.append
                            (fromDomEdit
                                { path = parentPath
                                , edit =
                                    DomEditAppendSubs
                                        ((updatedSubHead :: updatedSubsTail)
                                            |> List.map (\sub -> sub |> domFutureMap (\_ -> ()))
                                        )
                                }
                            )

        oldSubHead :: oldSubsTail ->
            case nodes.updated of
                [] ->
                    soFar
                        |> Rope.append
                            (fromDomEdit
                                { path = parentPath
                                , edit =
                                    DomEditRemoveLastNSubs
                                        (1 + (oldSubsTail |> List.length))
                                }
                            )

                updatedSubHead :: updatedSubsTail ->
                    domSubNodesDiffMapAtParentPathFromIndexAndSoFar
                        parentPath
                        (index + 1)
                        (domNodeDiffMapFrom soFar
                            (\innerEdit ->
                                fromDomEdit
                                    { path = index :: innerEdit.path
                                    , edit = innerEdit.edit
                                    }
                            )
                            { old = oldSubHead, updated = updatedSubHead }
                        )
                        fromDomEdit
                        { old = oldSubsTail, updated = updatedSubsTail }


domElementHeaderDiffMap :
    (DomEditSingle -> fromDomEdit)
    -> { old : DomElementHeader future, updated : DomElementHeader future }
    -> List fromDomEdit
domElementHeaderDiffMap fromDomEdit elements =
    { old = elements.old.styles, updated = elements.updated.styles }
        |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
            (\d -> d |> DomEditSetStyles |> fromDomEdit)
            Basics.identity
            Basics.identity
        |> List.LocalExtra.fromMaybe
        |> List.LocalExtra.consJust
            ({ old = elements.old.attributes, updated = elements.updated.attributes }
                |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                    (\d -> d |> DomEditSetAttributes |> fromDomEdit)
                    Basics.identity
                    Basics.identity
            )
        |> List.LocalExtra.consJust
            ({ old = elements.old.attributesNamespaced, updated = elements.updated.attributesNamespaced }
                |> sortedKeyValueListEditAndRemoveDiffMapBy namespacedKeyToComparable
                    (\d -> d |> DomEditSetAttributesNamespaced |> fromDomEdit)
                    (\entry -> { namespace = entry.key.namespace, key = entry.key.key, value = entry.value })
                    (\k -> { namespace = k.namespace, key = k.key })
            )
        |> List.LocalExtra.consJust
            ({ old = elements.old.stringProperties, updated = elements.updated.stringProperties }
                |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                    (\d -> d |> DomEditSetStringProperties |> fromDomEdit)
                    Basics.identity
                    Basics.identity
            )
        |> List.LocalExtra.consJust
            ({ old = elements.old.boolProperties, updated = elements.updated.boolProperties }
                |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                    (\d -> d |> DomEditSetBoolProperties |> fromDomEdit)
                    Basics.identity
                    Basics.identity
            )
        |> List.LocalExtra.consJust
            (if elements.old.scrollToPosition == elements.updated.scrollToPosition then
                Nothing

             else
                DomEditSetScrollToPosition elements.updated.scrollToPosition
                    |> fromDomEdit
                    |> Just
            )
        |> List.LocalExtra.consJust
            (if elements.old.scrollToShow == elements.updated.scrollToShow then
                Nothing

             else
                DomEditSetScrollToShow elements.updated.scrollToShow
                    |> fromDomEdit
                    |> Just
            )
        |> List.LocalExtra.consJust
            (case elements.old.scrollPositionRequest of
                Just _ ->
                    Nothing

                Nothing ->
                    case elements.updated.scrollPositionRequest of
                        Nothing ->
                            Nothing

                        Just _ ->
                            replacementDomElementScrollPositionRequest
                                |> fromDomEdit
                                |> Just
            )
        |> List.LocalExtra.consJust
            (let
                updatedElementEventListensId : List { key : String, value : DefaultActionHandling }
                updatedElementEventListensId =
                    elements.updated.eventListens
                        |> sortedKeyValueListToList
                        |> List.LocalExtra.mapAnyOrder
                            (\entry ->
                                { key = entry.key, value = entry.value.defaultActionHandling }
                            )
             in
             if
                (elements.old.eventListens
                    |> sortedKeyValueListToList
                    |> List.LocalExtra.mapAnyOrder
                        (\entry ->
                            { key = entry.key, value = entry.value.defaultActionHandling }
                        )
                )
                    == updatedElementEventListensId
             then
                Nothing

             else
                DomEditSetEventListens updatedElementEventListensId
                    |> fromDomEdit
                    |> Just
            )


replacementDomElementScrollPositionRequest : DomEditSingle
replacementDomElementScrollPositionRequest =
    DomEditRequestScrollPosition ()


sortedKeyValueListEditAndRemoveDiffMapBy :
    (key -> comparable_)
    -> ({ remove : List removeSingle, edit : List editSingle } -> fromRemoveAndEdit)
    -> ({ key : key, value : value } -> editSingle)
    -> (key -> removeSingle)
    ->
        { old : SortedKeyValueList key value
        , updated : SortedKeyValueList key value
        }
    -> Maybe fromRemoveAndEdit
sortedKeyValueListEditAndRemoveDiffMapBy keyToComparable fromEditAndRemove asDiffSingleEdit asDiffSingleRemove dicts =
    let
        diff : { remove : List removeSingle, edit : List editSingle }
        diff =
            sortedKeyValueListMergeBy keyToComparable
                (\removed soFar ->
                    { edit = soFar.edit
                    , remove = asDiffSingleRemove removed.key :: soFar.remove
                    }
                )
                (\old updated soFar ->
                    if old == updated.value then
                        soFar

                    else
                        { remove = soFar.remove
                        , edit = asDiffSingleEdit updated :: soFar.edit
                        }
                )
                (\updated soFar ->
                    { remove = soFar.remove
                    , edit = asDiffSingleEdit updated :: soFar.edit
                    }
                )
                (dicts.old |> sortedKeyValueListToList)
                (dicts.updated |> sortedKeyValueListToList)
                removeEmptyEditEmpty
    in
    case diff.remove of
        [] ->
            case diff.edit of
                [] ->
                    Nothing

                (_ :: _) as editsFilled ->
                    { remove = [], edit = editsFilled } |> fromEditAndRemove |> Just

        (_ :: _) as removeFilled ->
            { remove = removeFilled, edit = diff.edit } |> fromEditAndRemove |> Just


removeEmptyEditEmpty : { remove : List a_, edit : List b_ }
removeEmptyEditEmpty =
    { remove = [], edit = [] }


{-| Fold the lists of 2 [`SortedKeyValueList`](#SortedKeyValueList)s depending on where keys are present.
The idea and API is the same as [`Dict.merge`](https://dark.elm.dmy.fr/packages/elm/core/latest/Dict#merge)
-}
sortedKeyValueListMergeBy :
    (key -> comparable_)
    -> ({ key : key, value : a } -> folded -> folded)
    -> (a -> { key : key, value : b } -> folded -> folded)
    -> ({ key : key, value : b } -> folded -> folded)
    -> List { key : key, value : a }
    -> List { key : key, value : b }
    -> folded
    -> folded
sortedKeyValueListMergeBy keyToComparable onlyA bothAB onlyB aSortedKeyValueList bSortedKeyValueList initialFolded =
    case aSortedKeyValueList of
        [] ->
            bSortedKeyValueList |> List.foldl onlyB initialFolded

        aLowest :: aWithoutLowest ->
            case bSortedKeyValueList of
                [] ->
                    aWithoutLowest
                        |> List.foldl onlyA
                            (onlyA aLowest initialFolded)

                bLowest :: bWithoutLowest ->
                    case compare (aLowest.key |> keyToComparable) (bLowest.key |> keyToComparable) of
                        EQ ->
                            sortedKeyValueListMergeBy keyToComparable
                                onlyA
                                bothAB
                                onlyB
                                aWithoutLowest
                                bWithoutLowest
                                (bothAB aLowest.value bLowest initialFolded)

                        LT ->
                            sortedKeyValueListMergeBy keyToComparable
                                onlyA
                                bothAB
                                onlyB
                                aWithoutLowest
                                bSortedKeyValueList
                                (onlyA aLowest initialFolded)

                        GT ->
                            sortedKeyValueListMergeBy keyToComparable
                                onlyA
                                bothAB
                                onlyB
                                aSortedKeyValueList
                                bWithoutLowest
                                (onlyB bLowest initialFolded)


audioDiff :
    { old : Audio, updated : Audio }
    -> List AudioEditSingle
audioDiff audios =
    (if audios.old.volume == audios.updated.volume then
        []

     else
        [ AudioEditSetVolume audios.updated.volume ]
    )
        |> List.LocalExtra.consJust
            (if audios.old.speed == audios.updated.speed then
                Nothing

             else
                AudioEditSetSpeed audios.updated.speed
                    |> Just
            )
        |> List.LocalExtra.consJust
            (if audios.old.stereoPan == audios.updated.stereoPan then
                Nothing

             else
                AudioEditSetStereoPan audios.updated.stereoPan
                    |> Just
            )
        |> List.LocalExtra.consJust
            (if audios.old.processingLastToFirst == audios.updated.processingLastToFirst then
                Nothing

             else
                AudioEditSetProcessing
                    (audios.updated.processingLastToFirst
                        |> List.reverse
                    )
                    |> Just
            )


remove : InterfaceSingleDiff irrelevantFuture_
remove =
    Remove ()


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
    = EditDom (List { path : List Int, edit : DomEditSingle })
    | EditAudio
        { url : String
        , startTime : Time.Posix
        , edits :
            List
                AudioEditSingle
        }
    | EditNotification { id : String, message : String, details : String }


{-| Changes to an individual aspect of an [`Audio`](#Audio)
-}
type AudioEditSingle
    = AudioEditSetVolume AudioParameterTimeline
    | AudioEditSetSpeed AudioParameterTimeline
    | AudioEditSetStereoPan AudioParameterTimeline
    | AudioEditSetProcessing (List AudioProcessing)


{-| Some kind of sound we want to play. To create `Audio`, start with [`Web.audioFromSource`](Web#audioFromSource)
-}
type alias Audio =
    RecordWithoutConstructorFunction
        { url : String
        , startTime : Time.Posix
        , volume : AudioParameterTimeline
        , speed : AudioParameterTimeline
        , stereoPan : AudioParameterTimeline
        , processingLastToFirst : List AudioProcessing
        }


{-| A single effect filter applied to an [`Audio`](#Audio)
-}
type AudioProcessing
    = AudioLinearConvolution { sourceUrl : String }
    | AudioLowpass { cutoffFrequency : AudioParameterTimeline }
    | AudioHighpass { cutoffFrequency : AudioParameterTimeline }


{-| Audio data we can use to play sounds.
Use [`Web.audioSourceLoad`](Web#audioSourceLoad) to fetch an [`AudioSource`](#AudioSource).

You can also use the contained source `duration`, for example to find fade-out times or to create a loop:

    audioLoop : AudioSource -> Time.Posix -> Time.Posix -> Audio
    audioLoop source initialTime lastTick =
        Web.audioFromSource source
            (Duration.addTo
                initialTime
                (source.duration
                    |> Quantity.multiplyBy
                        (((Duration.from initialTime lastTick |> Duration.inSeconds)
                            / (source.duration |> Duration.inSeconds)
                         )
                            |> floor
                            |> toFloat
                        )
                )
            )

-}
type alias AudioSource =
    RecordWithoutConstructorFunction
        { url : String
        , duration : Duration
        }


{-| defining how loud a sound should be at any point in time
-}
type alias AudioParameterTimeline =
    { startValue : Float
    , keyFrames : List { time : Time.Posix, value : Float }
    }


{-| What parts of a node are replaced. Either all modifiers of a certain kind or the whole node
-}
type DomEditSingle
    = DomEditReplaceNode (DomNode ())
    | DomEditAppendSubs (List (DomNode ()))
    | DomEditRemoveLastNSubs Int
    | DomEditSetStyles { edit : List { key : String, value : String }, remove : List String }
    | DomEditSetAttributes { edit : List { key : String, value : String }, remove : List String }
    | DomEditSetAttributesNamespaced { edit : List { namespace : String, key : String, value : String }, remove : List { namespace : String, key : String } }
    | DomEditSetStringProperties { edit : List { key : String, value : String }, remove : List String }
    | DomEditSetBoolProperties { edit : List { key : String, value : Bool }, remove : List String }
    | DomEditSetScrollToPosition (Maybe { fromLeft : Float, fromTop : Float })
    | DomEditSetScrollToShow (Maybe { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment })
    | DomEditRequestScrollPosition ()
    | DomEditSetEventListens (List { key : String, value : DefaultActionHandling })


{-| Create a [`Program`](#Program):

  - The state is everything the program knows (what The Elm Architecture calls model).
    And it always starts with a given `initialState`

  - The [`Interface`](#Interface) is the face to the outside world
    and can be created using the helpers in [time](#time), [DOM](#dom), [HTTP](#http-client) etc.
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
    let
        initialStateCmdTuplePreComputed : ( ProgramState state, Cmd (ProgramEvent state) )
        initialStateCmdTuplePreComputed =
            programInit appConfig
    in
    Platform.worker
        { init = \() -> initialStateCmdTuplePreComputed
        , update = \event state -> programUpdate appConfig event state
        , subscriptions = \state -> programSubscriptions appConfig state
        }


{-| A [`Platform.Program`](https://dark.elm.dmy.fr/packages/elm/core/latest/Platform#Program)
that elm can run,
produced by [`Web.program`](#program)
-}
type alias Program state =
    Platform.Program () (ProgramState state) (ProgramEvent state)


interfaceFromSingle : InterfaceSingle future -> Interface future
interfaceFromSingle interfaceSingle =
    FastDict.singleton
        (interfaceSingle |> interfaceSingleToStructuredId |> StructuredId.toString)
        interfaceSingle


{-| An [`Interface`](Web#Interface) for getting the current [POSIX time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix).

Replacement for [`elm/time`'s `Time.now`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#now).

-}
timePosixRequest : Interface Time.Posix
timePosixRequest =
    TimePosixRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for getting a [`Time.Zone`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Zone)
based on the current UTC offset.

Replacement for [`elm/time`'s `Time.here`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#here).

-}
timeZoneRequest : Interface Time.Zone
timeZoneRequest =
    TimezoneOffsetRequest (\offset -> Time.customZone -offset [])
        |> interfaceFromSingle


{-| Intended for package authors.
An [`Interface`](Web#Interface) for using [`Intl.DateTimeFormat().resolvedOptions().timeZone`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/resolvedOptions#timezone)
to get names like `Europe/Moscow` or `America/Havana`.
From there you can look it up in any [IANA data](https://www.iana.org/time-zones) you loaded yourself.

Replacement for [`elm/time`'s `Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
timeZoneNameRequest : Interface String
timeZoneNameRequest =
    TimezoneNameRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for getting a reminder
once a given [point in time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix) has been reached.

This lets you for example wait until it's 15 minutes before the event,
timeout a request or schedule a certain action to a specific time.

    import Web

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
                    [ Web.timePosixRequest
                        |> Web.interfaceFutureMap BeforeTimeout
                    , ..request.. |> Web.futureMap GotResult
                    ]

                BeforeTimeout requestTime ->
                    -- timeout after 10 seconds
                    Web.timeOnceAt (Duration.addTo requestTime (Duration.seconds 10))
                        |> Web.interfaceFutureMap (\_ -> TimedOut)

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
        -> Web.Interface (RequestState result)

where the result can be put into the "main state" and therefore cased on.

-}
timeOnceAt : Time.Posix -> Interface Time.Posix
timeOnceAt pointInTime =
    TimeOnce { pointInTime = pointInTime, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for getting the current time
every time a given [`Duration`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration) has passed.

Note: Do not use it for animations.
[`Web.animationFrameListen`](Web#animationFrameListen)
syncs up with repaints and will end up being much smoother for any moving visuals.

-}
timePeriodicallyListen : Duration -> Interface Time.Posix
timePeriodicallyListen intervalDuration =
    TimePeriodicallyListen
        { intervalDurationMilliSeconds = intervalDuration |> Duration.inMilliseconds |> Basics.round
        , on = identity
        }
        |> interfaceFromSingle


{-| Create an SVG [DOM node](#DomNode).
with a given tag, [`DomModifier`](Web#DomModifier)s and sub-nodes
-}
svgElement : String -> List (DomModifier future) -> List (DomNode future) -> DomNode future
svgElement tag modifiers subs =
    domElementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs


{-| An [`Interface`](Web#Interface) for detecting when data has been sent from the server at a given address
or the connection has changed, see [`Web.SocketEvent`](Web#SocketEvent).

You also have the ability to send data to the server whenever necessary.
It's common to pair this with [`Json.Encode.encode 0`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Encode#encode)
to send json.

    case state.answer of
        Nothing ->
            Web.socketListenAndMaybeSend
                { address = "ws://127.0.0.1:9000"
                , dataToSend = Just "Are you still on?"
                }
                |> Web.interfaceFutureMap
                    (\socketEvent ->
                        case socketEvent of
                            Web.SocketDataReceived answer ->
                                { state | answer = Just answer }

                            Web.SocketOpened ->
                                state

                            Web.SocketClosed _ ->
                                state
                    )

        Just _ ->
            Web.domRender (Web.domText "Just wanted to hear your voice, thx bye!")

"Removing" [`Web.socketListenAndMaybeSend`](#socketListenAndMaybeSend) from your interface will close the connection.

-}
socketListenAndMaybeSend : { address : String, dataToSend : Maybe String } -> Interface SocketEvent
socketListenAndMaybeSend info =
    let
        listenInterface : Interface SocketEvent
        listenInterface =
            SocketListen { address = info.address, on = identity }
                |> interfaceFromSingle
    in
    case info.dataToSend of
        Nothing ->
            listenInterface

        Just dataToSend ->
            interfaceBatch2
                listenInterface
                (SocketDataSend { address = info.address, data = dataToSend }
                    |> interfaceFromSingle
                )


{-| An [`Interface`](Web#Interface) for generating a given count of cryptographically sound unsigned 32-bit `Int`s.
You can use these in all kinds of packages that allow creating an initial seed
from ints like [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/Random-Pcg-Extended#initialSeed)

Note: uses [`window.crypto.getRandomValues`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)

-}
randomUnsignedInt32sRequest : Int -> Interface (List Int)
randomUnsignedInt32sRequest count =
    RandomUnsignedInt32sRequest { count = count, on = identity }
        |> interfaceFromSingle


{-| Ask the user to consent to receiving notifications, if they haven't already.

For security reasons, browsers require some kind of user interaction like a button click first.
So you could for example add a toggle to send notifications and ask only for permission
when the toggle is set.

-}
notificationAskForPermission : Interface future_
notificationAskForPermission =
    NotificationAskForPermission ()
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for pushing a notification to the user.
Users can in the future [respond to it by clicking it](Web#NotificationClicked).

To not notify users multiple times for multiple messages of the same kind,
choose an `id` with a description for this kind
so that the notification with the same kind gets silently replaced if it exists.

    case messagesFromOpponent of
        [] ->
            Web.interfaceNone

        [ onlyMessage ] ->
            Web.notificationShow
                { id = "opponent messages"
                , message = "Your opponent left a message"
                , details = "\"" ++ onlyMessage ++ "\""
                }

        _ :: _ :: message2Up ->
            Web.notificationShow
                { id = "opponent messages"
                , message =
                    [ "Your opponent left "
                    , 2 + (message2Up |> List.length) |> String.fromInt
                    , " messages"
                    ]
                        |> String.join
                , details = ""
                }

Another example of a reminder shown in advance

    let
        untilMeeting : Duration
        untilMeeting =
            Duration.from meetingTime currentTime
    in
    if untilMeeting |> Quantity.isLessThan (Duration.minutes 10) then
        Web.notificationShow
            { id = "time until meeting"
            , message =
                [ "the meeting starts in "
                , untilMeeting |> Duration.inMinutes |> Basics.ceiling |> String.fromInt)
                , " minutes"
                ]
                    |> String.concat
            , details = ""
            }

    else
        Web.interfaceNone

Note: If you haven't previously used [`notificationAskForPermission`](#notificationAskForPermission)
it will request this permission now.

-}
notificationShow :
    { message : String, details : String, id : String }
    -> Interface NotificationClicked
notificationShow content =
    NotificationShow
        { id = content.id
        , message = content.message
        , details = content.details
        , on = identity
        }
        |> interfaceFromSingle


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
    HttpRequestSend
        { url = request.url
        , method = request.method
        , headers = request.headers
        , bodyAsciiString =
            request.body
                |> Maybe.map AsciiString.fromBytes
        , on = identity
        }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for downloading a given file
with [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/) as its content,
a given type and and a given default name.

Replacement for [`File.Download.bytes`](https://dark.elm.dmy.fr/packages/elm/file/latest/File-Download#bytes)

-}
fileDownloadBytes : { name : String, mimeType : String, content : Bytes } -> Interface future_
fileDownloadBytes fileDownloadConfig =
    FileDownload
        { name = fileDownloadConfig.name
        , mimeType = fileDownloadConfig.mimeType
        , contentAsciiString = fileDownloadConfig.content |> AsciiString.fromBytes
        }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for printing a message with general information
like if certain tasks have been successful

> survey submitted and received successfully

Depending on what minifying tools you use for your production build, these might get removed.

Note: uses [`console.log`](https://developer.mozilla.org/en-US/docs/Web/API/console/log_static),
just like [`Debug.log`](https://dark.elm.dmy.fr/packages/elm/core/latest/Debug#log)

-}
consoleLog : String -> Interface future_
consoleLog string =
    ConsoleLog string
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for printing a message that something didn't succeed but you could recover from, for example

> ⚠️ Unknown device - there may be compatibility issues.

> ⚠️ Recoverable upload failure, will retry. Error was: no status.

Note: uses [`console.warn`](https://developer.mozilla.org/en-US/docs/Web/API/console/warn_static)

-}
consoleWarn : String -> Interface future_
consoleWarn string =
    ConsoleWarn string
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for printing a message that something failed with bad consequences, for example

> ❌ decoding the selected file failed. Please report this bug at ...

Note: uses [`console.error`](https://developer.mozilla.org/en-US/docs/Web/API/console/error_static)

-}
consoleError : String -> Interface future_
consoleError string =
    ConsoleError string
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for reading the textual contents of the system clipboard.

Note: uses [`navigator.clipboard.readText`](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/readText)

-}
clipboardRequest : Interface String
clipboardRequest =
    ClipboardRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for setting the textual contents of the system clipboard.

Note: uses [`navigator.clipboard.writeText`](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText)

-}
clipboardReplaceBy : String -> Interface future_
clipboardReplaceBy replacement =
    ClipboardReplaceBy replacement
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for reading the value of the entry with the given key.
Comes back with `Nothing` if that key doesn't exist.

    import Json.Decode
    import Web

    projectFromLocalStorageRequest : Web.Interface (Result String Project)
    projectFromLocalStorageRequest =
        Web.localStorageRequest "project"
            |> Web.interfaceFutureMap
                (\savedProject ->
                    case savedProject of
                        Nothing ->
                            "nothing had been saved" |> Err

                        Just savedProjectJsonString ->
                            savedProjectJsonString
                                |> Json.Decode.decodeString projectJsonDecoder
                                |> Result.mapError Json.Decode.errorToString
                )

-}
localStorageRequest : String -> Interface (Maybe String)
localStorageRequest key =
    LocalStorageRequest { key = key, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for replacing the value of the entry with the given key
or adding key and value as a new entry if the key doesn't exist.

    import Json.Decode
    import Web

    projectSaveToLocalStorage : Project -> Web.Interface future_
    projectSaveToLocalStorage =
        \project ->
            Web.localStorageSet "project"
                (project |> projectJsonEncode |> Json.Encode.encode 0)

Note: This will trigger an event for all other tabs of the same url origin
that can be listened to using [`localStorageSetOnADifferentTabListen`](#localStorageSetOnADifferentTabListen)

-}
localStorageSet : String -> String -> Interface future_
localStorageSet key newOrReplacementValue =
    LocalStorageSet { key = key, value = Just newOrReplacementValue }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for deleting the entry with the given key if it exists.

Note: This will trigger an event for all other tabs of the same url origin
that can be listened to using [`localStorageRemoveOnADifferentTabListen`](#localStorageRemoveOnADifferentTabListen)

-}
localStorageRemove : String -> Interface future_
localStorageRemove key =
    LocalStorageSet { key = key, value = Nothing }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for keeping an eye on
when the local storage on a different tab with the same url origin is removed.
-}
localStorageRemoveOnADifferentTabListen : String -> Interface AppUrl
localStorageRemoveOnADifferentTabListen key =
    LocalStorageRemoveOnADifferentTabListen { key = key, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for keeping an eye on
when the local storage on a different tab with the same url origin is set.

When the `oldValue` is `Nothing`, no entry with that key existed.

-}
localStorageSetOnADifferentTabListen : String -> Interface { appUrl : AppUrl, oldValue : Maybe String, newValue : String }
localStorageSetOnADifferentTabListen key =
    LocalStorageSetOnADifferentTabListen { key = key, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for setting the document's title
-}
titleReplaceBy : String -> Interface future_
titleReplaceBy titleReplacement =
    DocumentTitleReplaceBy titleReplacement
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for adding or replacing the document's author metadata
-}
authorSet : String -> Interface future_
authorSet authorName =
    DocumentAuthorSet authorName
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for adding or replacing the document's keywords metadata
which should consist of words relevant to the page's content
-}
keywordsSet : List String -> Interface future_
keywordsSet authorName =
    DocumentKeywordsSet authorName
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for adding or replacing the document's description metadata
which should be a short and accurate summary of the content of the page.
Several browsers, like Firefox and Opera, use this as the default description of bookmarked pages.
-}
descriptionSet : String -> Interface future_
descriptionSet authorName =
    DocumentDescriptionSet authorName
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting a specific [`document` event](https://developer.mozilla.org/en-US/docs/Web/API/Document#events)
that has no native [`Interface`](Web#Interface), like like scroll, scrollend, selectionchange or paste
-}
documentListenTo : String -> Interface Json.Decode.Value
documentListenTo eventName =
    DocumentEventListen { eventName = eventName, on = Json.Decode.value }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting a specific [`window` event](https://developer.mozilla.org/en-US/docs/Web/API/Window#events)
-}
windowListenTo : String -> Interface Json.Decode.Value
windowListenTo eventName =
    WindowEventListen { eventName = eventName, on = Json.Decode.value }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting changes to the [visibility to the user](Web#WindowVisibility)

You can use times where the page becomes hidden to for example pause a currently running game.
These times will also be the last reliable observation you can make before a user might close the page, so treat it as the likely end of the user's session

-}
windowVisibilityChangeListen : Interface WindowVisibility
windowVisibilityChangeListen =
    WindowVisibilityChangeListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for getting the inner window width and height in pixels,
not including toolbars/scrollbars
-}
windowSizeRequest : Interface { width : Int, height : Int }
windowSizeRequest =
    WindowSizeRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting changes to the inner window width and height
-}
windowSizeChangeListen : Interface { width : Int, height : Int }
windowSizeChangeListen =
    WindowEventListen
        { eventName = "resize"
        , on =
            Json.Decode.field "target"
                (Json.Decode.map2 (\width height -> { width = width, height = height })
                    (Json.Decode.field "innerWidth" Json.Decode.int)
                    (Json.Decode.field "innerHeight" Json.Decode.int)
                )
        }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting when animation frames occur.
This will be about 60 times per second, though 75, 120, and 144 are also widely used.
To balance this out in your animation, the [current time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix) is provided each frame.

To get a delta, you could use [`Web.timePosixRequest`](Web#timePosixRequest)
to get a start time and check with e.g. [`Duration.from`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration#from) how far you've progressed in the timeline.

Note: To improve performance and battery life, most browsers pause these notifications when the app is running in a background tab or a hidden `<iframe>`.

Replacement for [`Browser.Events.onAnimationFrame`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Events#onAnimationFrame)

Note: uses [`window.requestAnimationFrame`](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame).

-}
animationFrameListen : Interface Time.Posix
animationFrameListen =
    WindowAnimationFrameListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for reading the languages the user prefers.
Each described using language tags according to [RFC 5646: Tags for Identifying Languages (also known as BCP 47)](https://datatracker.ietf.org/doc/html/rfc5646).
In the returned list they are ordered by preference with the most preferred language first.

Note: uses [`window.navigator.languages`](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/languages)

-}
preferredLanguagesRequest : Interface (List String)
preferredLanguagesRequest =
    WindowPreferredLanguagesRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting changes to the languages the user prefers.
Each described using language tags according to [RFC 5646: Tags for Identifying Languages (also known as BCP 47)](https://datatracker.ietf.org/doc/html/rfc5646).
In the returned list they are ordered by preference with the most preferred language first.

Note: uses [`window.onlanguagechange`](https://developer.mozilla.org/en-US/docs/Web/API/Window/languagechange_event)

-}
preferredLanguagesChangeListen : Interface (List String)
preferredLanguagesChangeListen =
    WindowPreferredLanguagesChangeListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for checking
if a given [CSS @media query](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_media_queries) matches.
For example

    Web.mediaQueryRequest "(prefers-color-scheme: dark)"
    Web.mediaQueryRequest "(orientation: landscape)"
    Web.mediaQuery "(prefers-reduced-motion) and (prefers-contrast: more)"
    Web.mediaQueryRequest "(100px < width < 700px)"

Note: uses [`window.matchMedia(_).matches`](https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryList/matches)

-}
mediaQueryRequest : String -> Interface Bool
mediaQueryRequest queryStringToCheckIfMatches =
    MediaQueryRequest
        { queryString = queryStringToCheckIfMatches |> mediaQueryStringNormalize
        , on = identity
        }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for watching for when
a given [CSS @media query](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_media_queries)
starts to match and stops to match.

    Web.mediaQueryRequest "(prefers-color-scheme: dark)"
        |> Web.futureMap
            (\enableDarkMode ->
                { state
                    | configuredBackgroundColor =
                        if enableDarkMode then
                            Color.black

                        else
                            Color.white
                }
            )

Note: listens to the [`window.matchMedia(_).*Listener(...)`](https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryList#instance_methods)

-}
mediaQueryChangeListen : String -> Interface Bool
mediaQueryChangeListen queryStringToCheckIfMatches =
    MediaQueryChangeListen
        { queryString = queryStringToCheckIfMatches
        , on = identity
        }
        |> interfaceFromSingle


mediaQueryStringNormalize : String -> String
mediaQueryStringNormalize queryStringRaw =
    if
        (queryStringRaw |> String.startsWith "(")
            || (queryStringRaw |> String.endsWith ")")
    then
        queryStringRaw

    else
        "(" ++ queryStringRaw ++ ")"


{-| An [`Interface`](Web#Interface) for getting the current page's [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/).
Is usually used while starting up the app.

Note: Uses [`window.location.href`](https://developer.mozilla.org/en-US/docs/Web/API/Window/location)

-}
urlRequest : Interface AppUrl
urlRequest =
    NavigationUrlRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for changing the [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/),
without triggering a page load or adding a new entry to the browser history.

This can be useful if you have search box and you want the ?search=hats in the URL to match without adding a history entry for every single key stroke.
Imagine how annoying it would be to click back thirty times and still be on the same page!

Replacement for [`Browser.Navigation.urlReplace`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#urlReplace)

-}
urlReplace : AppUrl -> Interface future_
urlReplace appUrl =
    NavigationReplaceUrl appUrl
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for changing the [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/)
and adding a new entry to the browser history
without triggering a page load.

Replacement for [`Browser.Navigation.urlPush`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#urlPush)

-}
urlPush : AppUrl -> Interface future_
urlPush appUrl =
    NavigationPushUrl appUrl
        |> interfaceFromSingle



-- elm/browser on "How do I manage URL from a Browser.element?" https://github.com/elm/browser/blob/master/notes/navigation-in-elements.md


{-| An [`Interface`](Web#Interface) for going forward a given number of pages.
If there are no more pages in the future, this will do nothing.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.forward`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#forward)

-}
navigateForward : Int -> Interface future_
navigateForward urlSteps =
    NavigationGo urlSteps
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for going back a given number of pages.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.back`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#back)

-}
navigateBack : Int -> Interface future_
navigateBack urlSteps =
    NavigationGo urlSteps
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for leaving the current page and loading the given [URL](https://dark.elm.dmy.fr/packages/elm/url/latest/).
This always results in a page load, even if the provided URL is the same as the current one.

    gotoElmWebsite : Web.Interface future_
    gotoElmWebsite =
        Web.navigateTo "https://elm-lang.org/"

Replacement for [`Browser.Navigation.load`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#load)

-}
navigateTo : String -> Interface future_
navigateTo url =
    NavigationLoad url
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for reloading the current page.
This always results in a page load!

Note: This may grab resources from the browser cache.

Replacement for [`Browser.Navigation.reload`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#reload)

-}
reload : Interface future_
reload =
    NavigationReload ()
        |> interfaceFromSingle


{-| If you used [`urlPush`](#urlPush) to update the URL with new history entries,
when the user clicks ← or → buttons (or you call [`navigateForward`](#navigateForward) or [`navigateBack`](#navigateBack) yourself),
the URL will change but your UI will not.

[`navigationListen`](#navigationListen) is an [`Interface`](Web#Interface) for detecting those URL changes and making ui changes as needed.

When the app itself initiates a url change with [`urlPush`](#urlPush) or [`urlReplace`](#urlReplace), no such event is triggered.

Note: This event is called ["popstate"](https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event) in js

-}
navigationListen : Interface AppUrl
navigationListen =
    WindowEventListen
        { eventName = "popstate"
        , on =
            Json.Decode.field "state"
                (Json.Decode.oneOf
                    [ Json.Decode.field "appUrl" appUrlJsonDecoder
                    , Json.Decode.null () |> Json.Decode.map (\() -> AppUrl.fromPath [])
                    ]
                )
        }
        |> interfaceFromSingle


appUrlJsonDecoder : Json.Decode.Decoder AppUrl
appUrlJsonDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\appUrlString ->
                case appUrlString |> stringToAppUrl of
                    Nothing ->
                        Json.Decode.fail "invalid app-specific URL"

                    Just appUrl ->
                        Json.Decode.succeed appUrl
            )


stringToAppUrl : String -> Maybe AppUrl
stringToAppUrl appUrlString =
    if appUrlString |> String.startsWith "/" then
        ("https://dummy.com" ++ appUrlString)
            |> Url.fromString
            |> Maybe.map AppUrl.fromUrl

    else
        Nothing


{-| An [`Interface`](Web#Interface) for getting which [gamepads](Web#Gamepad)
are connected and in which ways buttons and axes are activated.

The given `Dict` keys uniquely identify each device for the whole session.

-}
gamepadsRequest : Interface (Dict.Dict Int Gamepad)
gamepadsRequest =
    GamepadsRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting changes to which [gamepads](Web#Gamepad)
are connected and in which ways buttons and axes are activated.

The given `Dict` keys uniquely identify each device for the whole session,
so you can for example check for removed and added gamepads using `Dict.diff`
or track one host device in the state.

Implementation note:
The [web gamepad API](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad_API) does not offer a listener to detect changes.
So instead, we poll every 14ms (a bit more often than 60 times/second)
which is just a bit faster than Chrome's actual fetch rate (fetch rate is not part of the specification).

We want to avoid missed inputs before your next simulation tick,
so we just had to guess a good interval number.
<https://stackoverflow.com/a/51483510>

If you have issues with unresponsiveness, [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)

-}
gamepadsChangeListen : Interface (Dict.Dict Int Gamepad)
gamepadsChangeListen =
    GamepadsChangeListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for getting the current [position of the device](Web#GeoLocation)
-}
geoLocationRequest : Interface GeoLocation
geoLocationRequest =
    GeoLocationRequest identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting changes in the current [position of the device](Web#GeoLocation)
-}
geoLocationChangeListen : Interface GeoLocation
geoLocationChangeListen =
    GeoLocationChangeListen identity
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for displaying a given [DOM node](Web#DomNode)
-}
domRender : DomNode future -> Interface future
domRender domNode =
    DomNodeRender domNode
        |> interfaceFromSingle


{-| Wire events from this [DOM node](Web#DomNode) to a specific event, for example

    buttonUi "start"
        |> Web.domFutureMap (\Clicked -> StartButtonClicked)

with

    buttonUi : String -> Web.DomNode ButtonEvent
    buttonUi label =
        Web.domElement "button"
            [ Web.domListenTo "click"
                |> Web.domModifierFutureMap (\_ -> Clicked)
            ]
            [ Web.domText label ]

    type ButtonEvent
        = Clicked

-}
domFutureMap : (future -> mappedFuture) -> DomNode future -> DomNode mappedFuture
domFutureMap futureChange domElementToMap =
    case domElementToMap of
        DomText string ->
            DomText string

        DomElement element ->
            element |> domElementFutureMap futureChange |> DomElement


domElementFutureMap : (future -> mappedFuture) -> DomElement future -> DomElement mappedFuture
domElementFutureMap futureChange domElementToMap =
    { header = domElementToMap.header |> domElementHeaderFutureMap futureChange
    , subs =
        domElementToMap.subs |> List.map (\node -> node |> domFutureMap futureChange)
    }


{-| Plain text [DOM node](#DomNode)
-}
domText : String -> DomNode future_
domText =
    DomText


domElementWithMaybeNamespace :
    Maybe String
    -> String
    -> List (DomModifier future)
    -> List (DomNode future)
    -> DomNode future
domElementWithMaybeNamespace maybeNamespace tag modifiers subs =
    let
        modifiersFlat :
            { namespace : Maybe String
            , tag : String
            , scrollToPosition : Maybe { fromLeft : Float, fromTop : Float }
            , scrollToShow : Maybe { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment }
            , scrollPositionRequest : Maybe ({ fromLeft : Float, fromTop : Float } -> future)
            , eventListens : List { key : String, value : { on : Json.Decode.Value -> future, defaultActionHandling : DefaultActionHandling } }
            , styles : List { key : String, value : String }
            , stringProperties : List { key : String, value : String }
            , boolProperties : List { key : String, value : Bool }
            , attributesNamespaced : List { key : { namespace : String, key : String }, value : String }
            , attributes : List { key : String, value : String }
            }
        modifiersFlat =
            modifiers
                |> domModifierBatch
                |> Rope.foldl
                    (\modifier soFar ->
                        case modifier of
                            ScrollToPosition position ->
                                { scrollToPosition = position |> Just
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToShow = soFar.scrollToShow
                                , scrollPositionRequest = soFar.scrollPositionRequest
                                , eventListens = soFar.eventListens
                                , styles = soFar.styles
                                , stringProperties = soFar.stringProperties
                                , boolProperties = soFar.boolProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            ScrollToShow alignment ->
                                { scrollToShow = alignment |> Just
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToPosition = soFar.scrollToPosition
                                , scrollPositionRequest = soFar.scrollPositionRequest
                                , eventListens = soFar.eventListens
                                , styles = soFar.styles
                                , stringProperties = soFar.stringProperties
                                , boolProperties = soFar.boolProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            ScrollPositionRequest positionRequest ->
                                { scrollPositionRequest = positionRequest |> Just
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToPosition = soFar.scrollToPosition
                                , scrollToShow = soFar.scrollToShow
                                , eventListens = soFar.eventListens
                                , styles = soFar.styles
                                , stringProperties = soFar.stringProperties
                                , boolProperties = soFar.boolProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            Listen listen ->
                                { eventListens =
                                    { key = listen.eventName
                                    , value =
                                        { on = listen.on
                                        , defaultActionHandling = listen.defaultActionHandling
                                        }
                                    }
                                        :: soFar.eventListens
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToPosition = soFar.scrollToPosition
                                , scrollToShow = soFar.scrollToShow
                                , scrollPositionRequest = soFar.scrollPositionRequest
                                , styles = soFar.styles
                                , stringProperties = soFar.stringProperties
                                , boolProperties = soFar.boolProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            Style keyValue ->
                                { styles = keyValue :: soFar.styles
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToPosition = soFar.scrollToPosition
                                , scrollToShow = soFar.scrollToShow
                                , scrollPositionRequest = soFar.scrollPositionRequest
                                , eventListens = soFar.eventListens
                                , stringProperties = soFar.stringProperties
                                , boolProperties = soFar.boolProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            StringProperty keyValue ->
                                { stringProperties =
                                    keyValue :: soFar.stringProperties
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToPosition = soFar.scrollToPosition
                                , scrollToShow = soFar.scrollToShow
                                , scrollPositionRequest = soFar.scrollPositionRequest
                                , eventListens = soFar.eventListens
                                , styles = soFar.styles
                                , boolProperties = soFar.boolProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            BoolProperty keyValue ->
                                { boolProperties =
                                    keyValue :: soFar.boolProperties
                                , namespace = soFar.namespace
                                , tag = soFar.tag
                                , scrollToPosition = soFar.scrollToPosition
                                , scrollToShow = soFar.scrollToShow
                                , scrollPositionRequest = soFar.scrollPositionRequest
                                , eventListens = soFar.eventListens
                                , styles = soFar.styles
                                , stringProperties = soFar.stringProperties
                                , attributes = soFar.attributes
                                , attributesNamespaced = soFar.attributesNamespaced
                                }

                            Attribute keyValue ->
                                case keyValue.namespace of
                                    Just namespace ->
                                        { attributesNamespaced =
                                            { key = { namespace = namespace, key = keyValue.key }
                                            , value = keyValue.value
                                            }
                                                :: soFar.attributesNamespaced
                                        , namespace = soFar.namespace
                                        , tag = soFar.tag
                                        , scrollToPosition = soFar.scrollToPosition
                                        , scrollToShow = soFar.scrollToShow
                                        , scrollPositionRequest = soFar.scrollPositionRequest
                                        , eventListens = soFar.eventListens
                                        , styles = soFar.styles
                                        , stringProperties = soFar.stringProperties
                                        , boolProperties = soFar.boolProperties
                                        , attributes = soFar.attributes
                                        }

                                    Nothing ->
                                        { attributes =
                                            { key = keyValue.key, value = keyValue.value }
                                                :: soFar.attributes
                                        , namespace = soFar.namespace
                                        , tag = soFar.tag
                                        , scrollToPosition = soFar.scrollToPosition
                                        , scrollToShow = soFar.scrollToShow
                                        , scrollPositionRequest = soFar.scrollPositionRequest
                                        , eventListens = soFar.eventListens
                                        , styles = soFar.styles
                                        , stringProperties = soFar.stringProperties
                                        , boolProperties = soFar.boolProperties
                                        , attributesNamespaced = soFar.attributesNamespaced
                                        }
                    )
                    { namespace = maybeNamespace
                    , tag = tag
                    , scrollToPosition = Nothing
                    , scrollToShow = Nothing
                    , scrollPositionRequest = Nothing
                    , eventListens = []
                    , styles = []
                    , stringProperties = []
                    , boolProperties = []
                    , attributes = []
                    , attributesNamespaced = []
                    }
    in
    { header =
        { namespace = maybeNamespace
        , tag = tag
        , scrollToPosition = modifiersFlat.scrollToPosition
        , scrollToShow = modifiersFlat.scrollToShow
        , scrollPositionRequest = modifiersFlat.scrollPositionRequest
        , eventListens =
            modifiersFlat.eventListens |> sortedKeyValueListFromList
        , styles =
            modifiersFlat.styles |> sortedKeyValueListFromList
        , stringProperties =
            modifiersFlat.stringProperties |> sortedKeyValueListFromList
        , boolProperties =
            modifiersFlat.boolProperties |> sortedKeyValueListFromList
        , attributes =
            modifiersFlat.attributes |> sortedKeyValueListFromList
        , attributesNamespaced =
            modifiersFlat.attributesNamespaced |> sortedKeyValueListFromListBy namespacedKeyToComparable
        }
    , subs = subs
    }
        |> DomElement


{-| Sort a given list of { key, value } elements to create a [`SortedKeyValueList`](#SortedKeyValueList)
-}
sortedKeyValueListFromList : List { value : value, key : comparable } -> SortedKeyValueList comparable value
sortedKeyValueListFromList unsortedList =
    SortedKeyValueList (unsortedList |> List.sortBy .key)


{-| Sort a given list of { key, value } elements
by a given comparable representation of the key
to create a [`SortedKeyValueList`](Web#SortedKeyValueList)
-}
sortedKeyValueListFromListBy :
    (key -> comparable_)
    -> List { value : value, key : key }
    -> SortedKeyValueList key value
sortedKeyValueListFromListBy keyToComparable unsortedList =
    SortedKeyValueList
        (unsortedList
            |> List.sortBy (\entry -> entry.key |> keyToComparable)
        )


{-| Create a DOM element with a given tag, [`DomModifier`](#DomModifier)s and sub-[node](Web#DomNode)s.
For example to get `<p>flying</p>`

    Web.domElement "p"
        []
        [ Web.domText "flying" ]

To create an SVG element, use [`Web.svgElement`](Web#svgElement)

-}
domElement : String -> List (DomModifier future) -> List (DomNode future) -> DomNode future
domElement tag modifiers subs =
    domElementWithMaybeNamespace Nothing tag modifiers subs


{-| Create a DOM element with a given namespace, tag, [`DomModifier`](#DomModifier)s and sub-[node](Web#DomNode)s.
For example, [`Web.svgElement`](Web#svgElement) is defined as

    svgElement : String -> List (Modifier future) -> List (DomNode future) -> DomNode future
    svgElement tag modifiers subs =
        Web.domElementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs

-}
domElementNamespaced :
    String
    -> String
    -> List (DomModifier future)
    -> List (DomNode future)
    -> DomNode future
domElementNamespaced namespace tag modifiers subs =
    domElementWithMaybeNamespace (namespace |> Just) tag modifiers subs


{-| Setting of a [`Web.DomElement`](Web#DomElement).
To create one, use [`domAttribute`](#domAttribute), [`domStyle`](#domStyle), [`domListenTo`](#domListenTo) etc.
To combine multiple, use [`Web.domModifierBatch`](#domModifierBatch) and [`Web.domModifierNone`](#domModifierNone)

For example to get `<a href="https://elm-lang.org">elm</a>`

    Web.domElement "a"
        [ Web.domAttribute "href" "https://elm-lang.org" ]
        [ Web.domText "elm" ]

Btw: If you can think of a nicer name for this like "customization", "characteristic" or "aspect",
please [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new).


## attribute vs property

  - attribute: part of the HTML itself. E.g. `class` in `<div class="greeting"></div>`
  - property: an actual field on that js DOM object. E.g. `className` in `div.className = "greeting"`

But don't be surprised: There are cases where

  - only a property _or_ an attribute of a name exists
  - both exist but they have different effects

For example, trying to reset the text inside a a text input with

    Web.domAttribute "value" "user input"

    -- then later replace it with
    Web.domAttribute "value" ""

will only provide a _default value_ and has no effect on the currently written text,
so you'll have to use

    Web.domStringProperty "value" ""

Similarly for checkboxes:

    Web.domBoolProperty "checked" False

Maybe a rule of thumb is:
Use properties to set anything related to interactivity
and attributes for everything else.

If you have some opinions or better explanations,
please [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new).

-}
type alias DomModifier future =
    Rope (DomModifierSingle future)


{-| Combine multiple [`DomModifier`](#DomModifier)s into one.
-}
domModifierBatch : List (DomModifier future) -> DomModifier future
domModifierBatch modifiers =
    modifiers |> Rope.fromList |> Rope.concat


{-| Doing nothing as a [`DomModifier`](#DomModifier). These two examples are equivalent:

    Web.domModifierBatch
        [ a, Web.domModifierNone, b ]

and

    Web.domModifierBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
domModifierNone : DomModifier future_
domModifierNone =
    Rope.empty


{-| An individual [`DomModifier`](#DomModifier).
Create using [`domAttribute`](#domAttribute), [`domStyle`](#domStyle), [`domListenTo`](#domListenTo) etc.
-}
type DomModifierSingle future
    = Attribute { namespace : Maybe String, key : String, value : String }
    | StringProperty { key : String, value : String }
    | BoolProperty { key : String, value : Bool }
    | Style { key : String, value : String }
    | ScrollToPosition { fromLeft : Float, fromTop : Float }
    | ScrollToShow { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment }
    | ScrollPositionRequest ({ fromLeft : Float, fromTop : Float } -> future)
    | Listen
        { eventName : String
        , on : Json.Decode.Value -> future
        , defaultActionHandling : DefaultActionHandling
        }


{-| A key-value attribute [`DomModifier`](#DomModifier)
-}
domAttribute : String -> String -> DomModifier future_
domAttribute key value =
    { namespace = Nothing, key = key, value = value } |> Attribute |> Rope.singleton


{-| A key-string value DOM property [`DomModifier`](#DomModifier)
-}
domStringProperty : String -> String -> DomModifier future_
domStringProperty key value =
    { key = key, value = value } |> StringProperty |> Rope.singleton


{-| A key-bool value DOM property [`DomModifier`](#DomModifier)
-}
domBoolProperty : String -> Bool -> DomModifier future_
domBoolProperty key value =
    { key = key, value = value } |> BoolProperty |> Rope.singleton


{-| A namespaced key-value attribute [`DomModifier`](#DomModifier).
For example, you could define an SVG xlink href attribute as

    attributeXlinkHref : String -> Web.DomModifier msg
    attributeXlinkHref value =
        Web.domAttributeNamespaced "http://www.w3.org/1999/xlink" "xlink:href" value

-}
domAttributeNamespaced : String -> String -> String -> DomModifier future_
domAttributeNamespaced namespace key value =
    { namespace = namespace |> Just, key = key, value = value } |> Attribute |> Rope.singleton


{-| A key-value style [`DomModifier`](#DomModifier)
-}
domStyle : String -> String -> DomModifier future_
domStyle key value =
    { key = key, value = value } |> Style |> Rope.singleton


{-| Listen for a specific DOM event on the [`Web.DomElement`](Web#DomElement).
Use [`domModifierFutureMap`](#domModifierFutureMap) to wire this to a specific event.

If you want to override the browser's default behavior for that event,
use [`domListenToPreventingDefaultAction`](#domListenToPreventingDefaultAction).

Be aware that unlike default js behavior, this event will not be passed further
to parent dom elements (instead, it "stops propagation"/"doesn't bubble").
For example, if you have an element on top of some background frame
and both listen to a click event, only the button will be notified.

If you for reasons can't go without bubbling,
please [tell me about it in an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)
so I can make it more customizable or find alternatives.

-}
domListenTo : String -> DomModifier Json.Decode.Value
domListenTo eventName =
    { eventName = eventName
    , on = identity
    , defaultActionHandling = DefaultActionExecute
    }
        |> Listen
        |> Rope.singleton


{-| Like [`domListenTo`](#domListenTo) but [preventing the browser's default action](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault).

That's for example how elm's [`Browser.Events.onSubmit`](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#onSubmit)
prevents the form from changing the page’s location:

    submitListen : Web.DomModifier ()
    submitListen =
        Web.domListenToPreventingDefaultAction "submit"
            |> Web.domModifierFutureMap (\_ -> ())

-}
domListenToPreventingDefaultAction : String -> DomModifier Json.Decode.Value
domListenToPreventingDefaultAction eventName =
    { eventName = eventName
    , on = identity
    , defaultActionHandling = DefaultActionPrevent
    }
        |> Listen
        |> Rope.singleton


{-| Wire events from this [`DomModifier`](#DomModifier) to a specific event.

    Web.domListenTo "click" |> Web.domModifierFutureMap (\_ -> ButtonClicked)

-}
domModifierFutureMap :
    (future -> mappedFuture)
    -> DomModifier future
    -> DomModifier mappedFuture
domModifierFutureMap futureChange modifier =
    modifier
        |> ropeMapFast
            (\modifierSingle ->
                modifierSingle |> domModifierSingleMap futureChange
            )


ropeMapFast : (a -> b) -> Rope a -> Rope b
ropeMapFast elementChange rope =
    rope
        |> Rope.foldr
            (\element soFar -> elementChange element :: soFar)
            []
        |> Rope.fromList


domModifierSingleMap :
    (future -> mappedFuture)
    -> DomModifierSingle future
    -> DomModifierSingle mappedFuture
domModifierSingleMap futureChange modifier =
    case modifier of
        Attribute keyValue ->
            keyValue |> Attribute

        Style keyValue ->
            keyValue |> Style

        StringProperty keyValue ->
            keyValue |> StringProperty

        BoolProperty keyValue ->
            keyValue |> BoolProperty

        ScrollToPosition position ->
            position |> ScrollToPosition

        ScrollToShow alignment ->
            alignment |> ScrollToShow

        ScrollPositionRequest request ->
            (\future -> future |> request |> futureChange) |> ScrollPositionRequest

        Listen listen ->
            { eventName = listen.eventName
            , on = \json -> listen.on json |> futureChange
            , defaultActionHandling = listen.defaultActionHandling
            }
                |> Listen


{-| Getting the current scroll position from the left and top.

Use in combination with [`domScrollToPosition`](#domScrollToPosition)
to implement saving and restoring scroll position even when users had navigated off a URL.

-}
domScrollPositionRequest : DomModifier { fromLeft : Float, fromTop : Float }
domScrollPositionRequest =
    ScrollPositionRequest identity |> Rope.singleton


{-| Ensure a given initial scroll position in both directions.
To move to the edge in a direction, use [`domScrollToShow`](#domScrollToShow) instead.

Unlike [`domStyle`](#domStyle)s,
this is just an initial configuration
which can be changed by user actions.
So adding e.g. `scrollToPosition ...`
will scroll once the next render happens
but will not prevent users from scrolling away.

-}
domScrollToPosition :
    { fromLeft : Float, fromTop : Float }
    -> DomModifier future_
domScrollToPosition position =
    ScrollToPosition position |> Rope.singleton


{-| Ensure a given initial [`DomElementVisibilityAlignment`](Web#DomElementVisibilityAlignment)
in both directions.

Unlike [`domStyle`](#domStyle)s,
this is just an initial configuration
which can be changed by user actions.
So adding e.g. `scrollToShow { y = Web.DomElementStart, x = Web.DomElementStart }`
will scroll to the top left once the next render happens
but will not prevent users from scrolling away.

Note: Uses [`Element.scrollIntoView`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView)

-}
domScrollToShow :
    { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment }
    -> DomModifier future_
domScrollToShow preferredAlignment =
    ScrollToShow preferredAlignment |> Rope.singleton


{-| Plain text or an [`DomElement`](#DomElement). Create using [`domText`](#domText) and [`domElement`](#domElement)
-}
type DomNode future
    = DomText String
    | DomElement (DomElement future)


{-| A tagged DOM node that can itself contain child [node](#DomNode)s
-}
type alias DomElement future =
    RecordWithoutConstructorFunction
        { header : DomElementHeader future
        , subs : List (DomNode future)
        }


{-| Change the stereo panning with a given a signed percentage [parameter](Web#AudioParameterTimeline).

`Web.audioStereoPan -0.9` for example means that the sound is almost fully balanced towards the left speaker

-}
audioStereoPan : AudioParameterTimeline -> Audio -> Audio
audioStereoPan signedPercentageTimeline audio =
    { audio
        | stereoPan =
            audio.stereoPan
                |> audioParameterScaleAlongParameter audio.startTime
                    signedPercentageTimeline
    }


{-| Scale the playback rate by a given factor. This will also affect pitch.

For example, `Web.audioSpeedScaleBy 0.5` means playback will take twice as long and the pitch will be one octave lower, see [AudioBufferSourceNode.playbackRate](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/playbackRate)

In general, to pitch by semitones:

    Web.audioSpeedScaleBy
        (Web.audioParameterAt (2 ^ (semitones / 12)))

Note: It would be possible to modify the signal to compensate for the pitch change,
see [Audio time stretching and pitch scaling](https://en.wikipedia.org/wiki/Audio_time_stretching_and_pitch_scaling).
Help appreciated!

-}
audioSpeedScaleBy : AudioParameterTimeline -> Audio -> Audio
audioSpeedScaleBy speedScaleFactorTimeline audio =
    { audio
        | speed =
            audio.speed
                |> audioParameterScaleAlongParameter audio.startTime
                    speedScaleFactorTimeline
    }


{-| Scale how loud it is.
1 preserves the current volume, 0.5 halves it, and 0 mutes it.
If the the volume is less than 0, 0 will be used instead.
-}
audioVolumeScaleBy : AudioParameterTimeline -> Audio -> Audio
audioVolumeScaleBy volumeScaleFactor audio =
    { audio
        | volume =
            audio.volume
                |> audioParameterScaleAlongParameter audio.startTime
                    volumeScaleFactor
    }


audioAddProcessing : AudioProcessing -> Audio -> Audio
audioAddProcessing newLastProcessing audio =
    { audio
        | processingLastToFirst = audio.processingLastToFirst |> (::) newLastProcessing
    }


{-| Usually used to apply reverb and or echo.
Given a loaded [`AudioSource`](Web#AudioSource) containing the impulse response,
it performs a [Convolution](https://en.wikipedia.org/wiki/Convolution) with the [`Audio`](Web#Audio)

If you need some nice impulse wavs to try it out, there's a few at [`dhiogoboza/audio-convolution`](https://github.com/dhiogoboza/audio-convolution/tree/master/impulses).
If you know more nice ones, don't hesitate to open an issue or a PR.

-}
audioAddLinearConvolutionWith : AudioSource -> Audio -> Audio
audioAddLinearConvolutionWith bufferAudioSource audio =
    audio |> audioAddProcessing (AudioLinearConvolution { sourceUrl = bufferAudioSource.url })


{-| Frequencies below a given cutoff [parameter](Web#AudioParameterTimeline) pass through;
frequencies above it are attenuated.

Has a 12dB/octave rolloff and no peak at the cutoff.

-}
audioAddLowpassUntilFrequency : AudioParameterTimeline -> Audio -> Audio
audioAddLowpassUntilFrequency cutoffFrequency audio =
    audio |> audioAddProcessing (AudioLowpass { cutoffFrequency = cutoffFrequency })


{-| Frequencies below a given cutoff [parameter](Web#AudioParameterTimeline) are attenuated;
frequencies above it pass through.

Has a 12dB/octave rolloff and no peak at the cutoff.

-}
audioAddHighpassFromFrequency : AudioParameterTimeline -> Audio -> Audio
audioAddHighpassFromFrequency cutoffFrequency audio =
    audio |> audioAddProcessing (AudioHighpass { cutoffFrequency = cutoffFrequency })


{-| Create [`Audio`](Web#Audio) from an given loaded [source](Web#AudioSource)
which will play at a given [time](https://dark.elm.dmy.fr/packages/elm/time/latest/)

    -- play a song at half speed and wait 2 seconds after the usual song start time before starting
    Web.audioFromSource
        myCoolSong
        (Duration.addTo usualSongStartTime (Duration.seconds 2))
        |> Web.audioSpeedScaleBy (Web.audioParameterAt 0.5)

Note that in some browsers audio will be muted until the user interacts with the webpage.

-}
audioFromSource : AudioSource -> Time.Posix -> Audio
audioFromSource source startTime =
    { url = source.url
    , startTime = startTime
    , volume = audioParameterAt 1
    , speed = audioParameterAt 1
    , stereoPan = audioParameterAt 0
    , processingLastToFirst = []
    }


{-| An [`Interface`](Web#Interface) for fetching audio data from a given url
and returning an [`AudioSource`](Web#AudioSource) to use with [`audioFromSource`](#audioFromSource).
-}
audioSourceLoad : String -> Interface (Result AudioSourceLoadError AudioSource)
audioSourceLoad url =
    AudioSourceLoad { url = url, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for playing [`Audio`](Web#Audio) created with [`audioFromSource`](#audioFromSource).

To play multiple audios:

    [ audio0, audio1, audio2 ]
        |> List.map Web.audioPlay
        |> Web.interfaceBatch

-}
audioPlay : Audio -> Interface future_
audioPlay audio =
    AudioPlay
        { audio
            | startTime = Duration.addTo audio.startTime (Duration.milliseconds 50)
            , volume = audio.volume |> audioParameterValuesAlter (\value -> Basics.max 0 value)

            -- negative speed values are supported by some browsers
            -- https://stackoverflow.com/questions/9874167/how-can-i-play-audio-in-reverse-with-web-audio-api/9875011#9875011
        }
        |> interfaceFromSingle


audioParameterValuesAlter : (Float -> Float) -> AudioParameterTimeline -> AudioParameterTimeline
audioParameterValuesAlter valueAlter timeline =
    { startValue = timeline.startValue |> valueAlter
    , keyFrames =
        timeline.keyFrames
            |> List.map (\keyFrame -> { time = keyFrame.time, value = keyFrame.value |> valueAlter })
    }


{-| Set it to a constant value. Add [`audioParameterThrough`](#audioParameterThrough) to make it transition from this starting value over time
-}
audioParameterAt : Float -> AudioParameterTimeline
audioParameterAt valueAtTheStart =
    { startValue = valueAtTheStart, keyFrames = [] }


{-| Specify a key value at a given absolute point in time.
The parameter will then transition linearly between those points.

Let's define an audio function that fades in to 1 and then fades out until it's 0 again.

    import Duration
    import Time
    import Web


    -- 1                ________
    --                /         \
    -- 0 ____________/           \_______
    --    t ->    fade in     fade out
    fadeInOut fadeInStartTime fadeOutEndTime audio =
        Web.audioParameterAt 0
            |> Web.audioParameterThrough fadeInStartTime 1
            |> Web.audioParameterThrough (Duration.addTo fadeInStartTime Duration.second) 1
            |> Web.audioParameterThrough (Duration.subtractFrom fadeOutEndTime Duration.second) 1
            |> Web.audioParameterThrough fadeOutEndTime 0

  - 🧩 `Duration` is from [ianmackenzie/elm-units](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/)
  - 🧩 `Time` is from [elm/time](https://dark.elm.dmy.fr/packages/elm/time/latest/)

You do not have to worry about order.

-}
audioParameterThrough : Time.Posix -> Float -> AudioParameterTimeline -> AudioParameterTimeline
audioParameterThrough keyFrameMoment keyFrameValue audioParameterTimelineSoFar =
    { startValue = audioParameterTimelineSoFar.startValue
    , keyFrames =
        { time = keyFrameMoment, value = keyFrameValue } :: audioParameterTimelineSoFar.keyFrames
    }


audioParameterScaleAlongParameter : Time.Posix -> AudioParameterTimeline -> AudioParameterTimeline -> AudioParameterTimeline
audioParameterScaleAlongParameter startTime timelineToScaleBy audioParameterTimelineToScale =
    let
        startValue : Float
        startValue =
            audioParameterTimelineToScale.startValue * timelineToScaleBy.startValue
    in
    { startValue = startValue
    , keyFrames =
        audioParameterTimelineToScale.keyFrames
            |> List.sortBy (\keyFrame -> keyFrame.time |> Time.posixToMillis)
            |> audioParameterKeyFramesScaleAlong
                { previous = { time = startTime, value = startValue }
                , toScaleBy =
                    timelineToScaleBy.keyFrames
                        |> List.sortBy (\keyFrame -> keyFrame.time |> Time.posixToMillis)
                }
    }


audioParameterKeyFramesScaleAlong :
    { toScaleBy : List { time : Time.Posix, value : Float }
    , previous : { time : Time.Posix, value : Float }
    }
    -> List { time : Time.Posix, value : Float }
    -> List { time : Time.Posix, value : Float }
audioParameterKeyFramesScaleAlong state toScale =
    List.map2
        (\keyFrameToScale keyFrameToScaleBy ->
            { time = keyFrameToScale.time
            , value = keyFrameToScale.value * keyFrameToScaleBy.value
            }
        )
        (toScale |> audioParameterKeyFramesAddSubs { subs = state.toScaleBy |> List.map .time, previous = state.previous })
        (state.toScaleBy |> audioParameterKeyFramesAddSubs { subs = toScale |> List.map .time, previous = state.previous })


audioParameterKeyFramesAddSubs :
    { subs : List Time.Posix
    , previous : { time : Time.Posix, value : Float }
    }
    -> List { time : Time.Posix, value : Float }
    -> List { time : Time.Posix, value : Float }
audioParameterKeyFramesAddSubs state keyFrames =
    -- IGNORE TCO
    case state.subs of
        [] ->
            keyFrames

        currentSub :: afterCurrentSub ->
            case keyFrames of
                [] ->
                    (currentSub :: afterCurrentSub)
                        |> List.map (\subTime -> { time = subTime, value = state.previous.value })

                currentKeyFrame :: afterCurrentKeyFrame ->
                    case compare (currentKeyFrame.time |> Time.posixToMillis) (currentSub |> Time.posixToMillis) of
                        EQ ->
                            currentKeyFrame
                                :: (afterCurrentKeyFrame
                                        |> audioParameterKeyFramesAddSubs
                                            { subs = afterCurrentSub
                                            , previous = currentKeyFrame
                                            }
                                   )

                        LT ->
                            currentKeyFrame
                                :: (afterCurrentKeyFrame
                                        |> audioParameterKeyFramesAddSubs
                                            { subs = currentSub :: afterCurrentSub
                                            , previous = currentKeyFrame
                                            }
                                   )

                        GT ->
                            let
                                locationBetween : Float
                                locationBetween =
                                    ((currentKeyFrame.time |> Time.posixToMillis) - (currentSub |> Time.posixToMillis) |> Basics.toFloat)
                                        / ((currentKeyFrame.time |> Time.posixToMillis) - (state.previous.time |> Time.posixToMillis) |> Basics.toFloat)

                                subKeyFrame : { time : Time.Posix, value : Float }
                                subKeyFrame =
                                    { time = currentSub
                                    , value = linearlyInterpolate state.previous.value currentKeyFrame.value locationBetween
                                    }
                            in
                            subKeyFrame
                                :: (afterCurrentKeyFrame
                                        |> audioParameterKeyFramesAddSubs
                                            { subs = currentSub :: afterCurrentSub
                                            , previous = subKeyFrame
                                            }
                                   )


linearlyInterpolate : Float -> Float -> Float -> Float
linearlyInterpolate startValue endValue progress =
    if Basics.isInfinite progress then
        startValue

    else
        progress * (endValue - startValue) + startValue
