module Web exposing
    ( program, Program
    , Interface, interfaceBatch, interfaceNone, interfaceFutureMap
    , timePosixRequest, timeZoneRequest, timeZoneNameRequest
    , timePeriodicallyListen, timeOnceAt
    , DomElementHeader, DomElementVisibilityAlignment(..), DefaultActionHandling(..)
    , DomNode(..), domText
    , DomElement, domElement, svgElement, domElementNamespaced
    , domFutureMap, domRender
    , DomModifier, domModifierFutureMap, domModifierBatch, domModifierNone
    , domAttribute, domAttributeNamespaced, domStyle, domBoolProperty, domStringProperty
    , domListenTo, domListenToPreventingDefaultAction
    , domScrollToShow, domScrollPositionRequest, domScrollToPosition
    , urlRequest
    , pushUrl, replaceUrl
    , navigateForward, navigateBack, navigationListen
    , navigateTo, reload
    , HttpRequest, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , httpRequest
    , httpGet, httpPost, httpAddHeaders
    , httpExpectString, httpExpectJson, httpExpectBytes, httpExpectWhatever
    , httpBodyJson, httpBodyBytes
    , randomUnsignedInt32s
    , consoleLog, consoleWarn, consoleError
    , localStorageRequest, localStorageSet, localStorageRemove
    , localStorageSetOnADifferentTabListen, localStorageRemoveOnADifferentTabListen
    , SocketConnectionEvent(..), SocketId(..)
    , socketConnectTo, socketDisconnect
    , socketMessage, socketMessageListen
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
    , animationFrameListen, windowVisibilityChangeListen, WindowVisibility(..)
    , windowSizeRequest, windowResizeListen
    , preferredLanguagesRequest, preferredLanguagesChangeListen
    , documentListenTo, windowListenTo
    , titleReplaceBy, authorSet, keywordsSet, descriptionSet
    , ProgramConfig, programInit, programUpdate, programSubscriptions
    , DomModifierSingle(..)
    , ProgramState(..), ProgramEvent(..), InterfaceSingle(..), DomTextOrElementHeader(..)
    , SortedKeyValueList(..)
    , InternalFastDict(..), InternalFastDictInner(..), InternalFastDictNColor(..)
    , interfaceSingleEdits, InterfaceSingleEdit(..), AudioEdit(..), DomEdit(..)
    )

{-| A state-interface program that can run in the browser

@docs program, Program

You can also [embed](#embed) a state-interface program as part of an existing app that uses The Elm Architecture.


# interface

@docs Interface, interfaceBatch, interfaceNone, interfaceFutureMap


## time

[`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/) primitives as part of an [`Interface`](Web#Interface).

@docs timePosixRequest, timeZoneRequest, timeZoneNameRequest
@docs timePeriodicallyListen, timeOnceAt


## DOM

These are primitives used for SVG and HTML
(filling the same role as [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/))

@docs DomElementHeader, DomElementVisibilityAlignment, DefaultActionHandling

@docs DomNode, domText
@docs DomElement, domElement, svgElement, domElementNamespaced
@docs domFutureMap, domRender
@docs DomModifier, domModifierFutureMap, domModifierBatch, domModifierNone
@docs domAttribute, domAttributeNamespaced, domStyle, domBoolProperty, domStringProperty
@docs domListenTo, domListenToPreventingDefaultAction
@docs domScrollToShow, domScrollPositionRequest, domScrollToPosition


## navigation

`history` interaction as part of an [`Interface`](Web#Interface)

@docs urlRequest
@docs pushUrl, replaceUrl
@docs navigateForward, navigateBack, navigationListen
@docs navigateTo, reload


## HTTP

Helpers for HTTP requests as part of an [`Interface`](Web#Interface)

@docs HttpRequest, HttpBody, HttpExpect, HttpError, HttpMetadata

@docs httpRequest
@docs httpGet, httpPost, httpAddHeaders
@docs httpExpectString, httpExpectJson, httpExpectBytes, httpExpectWhatever
@docs httpBodyJson, httpBodyBytes


## random

Helpers for randomness as part of an [`Interface`](Web#Interface).
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
                    Web.randomUnsignedInt32s 4
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

@docs randomUnsignedInt32s


## console

Helpers for console interactions as part of an [`Interface`](Web#Interface)

@docs consoleLog, consoleWarn, consoleError


## local storage

Saved data for the url origin (protocol, host name, port) across browser sessions.

This data doesn't expire and won't be cleared when the page is closed.
The only exception is "incognito mode", where all data is cleared once the last "private" tab is closed.

see [mdn on `Window.localStorage`](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage)

@docs localStorageRequest, localStorageSet, localStorageRemove
@docs localStorageSetOnADifferentTabListen, localStorageRemoveOnADifferentTabListen


## socket

Helpers for web sockets as part of an [`Interface`](Web#Interface)

@docs SocketConnectionEvent, SocketId


### connection

@docs socketConnectTo, socketDisconnect


### communicate

@docs socketMessage, socketMessageListen


## File download

Helpers for downloading a dynamically generated file as part of an [`Interface`](Web#Interface).

Security note: Browsers require downloads to be initiated by a user event.
So rather than allowing malicious sites to put files on your computer however they please,
the user has to at least click a button first.
As a result, the following interfaces only work when they are triggered by some user event.

Note: There's no equivalent module for file select
since you can easily replicate the behavior using an input element with type file or file drop area modifiers,
see for example [mpizenberg/elm-file](https://dark.elm.dmy.fr/packages/mpizenberg/elm-file/latest/FileValue#load-files-with-an-input).

@docs fileDownloadBytes


## audio

Play sounds and music as part of an [`Interface`](Web#Interface).

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

Helpers for clipboard interactions as part of an [`Interface`](Web#Interface)

@docs clipboardRequest, clipboardReplaceBy

Note: To listen for [copy, cut and paste events](https://developer.mozilla.org/en-US/docs/Web/API/ClipboardEvent),
use [`Web.domListenTo`](Web#domListenTo)


## geo location

Observe the [`GeoLocation`](Web#GeoLocation) as part of an [`Interface`](Web#Interface)
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


## Notification

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


## window

Observe and alter the page's global environment as part of an [`Interface`](Web#Interface)

@docs animationFrameListen, windowVisibilityChangeListen, WindowVisibility
@docs windowSizeRequest, windowResizeListen
@docs preferredLanguagesRequest, preferredLanguagesChangeListen
@docs documentListenTo, windowListenTo

When navigating to a new page on the same site,
you may want to change the document's context:

@docs titleReplaceBy, authorSet, keywordsSet, descriptionSet


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

@docs DomModifierSingle

@docs ProgramState, ProgramEvent, InterfaceSingle, DomTextOrElementHeader

@docs SortedKeyValueList

@docs InternalFastDict, InternalFastDictInner, InternalFastDictNColor

@docs interfaceSingleEdits, InterfaceSingleEdit, AudioEdit, DomEdit

If you need more things like json encoders/decoders, [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new)

-}

import Angle exposing (Angle)
import AppUrl exposing (AppUrl)
import AppUrl.LocalExtra
import Bytes exposing (Bytes)
import Bytes.LocalExtra
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
import Result.LocalExtra
import Rope exposing (Rope)
import Rope.LocalExtra
import Speed exposing (Speed)
import StructuredId exposing (StructuredId)
import Time
import Time.LocalExtra
import Url.LocalExtra



{- Hey! Here's how to add a new interface:


     - choose a name with roughly the shape `DomainSubjectVerb`

     - to `Web.InterfaceSingle`, add a variant `| [YourName] ..additional info (and future handles)..`

     - inside `Web.interfaceSingleFutureJsonDecoder`, specify what js values you expect to decode

     - inside `Web.interfaceSingleToStructuredId`, assign a unique identifier to your interface

       This helps recognize when interfaces have been added, have changed or have been deleted.
       Unless you want to allow your interface to be edited while it's running,
       it usually contains all the info from the `DomainSubjectVerb` variant (not including functions)

     - in `runner/index.ts` inside `interfaceAddImplementation`, add
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

    - inside `Web.interfaceSingleEdits`, add a case `[YourName] -> []`.
      If your running interface can be changed, read the section below


   Sometimes, removing + adding the new interface would not be the same as editing the existing one or would at least perform worse.
   For example, changing the volume of an audio should not require removing and and re-adding all audio nodes.

   If you also want to enable editing a running interface:

    - to `Web.InterfaceSingleEdit`, add a variant `| Edit[YourName] ..your diff info..`
    - inside `Web.interfaceSingleEdits`, set the case
      ```elm
      [YourName] old ->
          case case interfaces.updated of
              [YourName] new ->
                  Edit[YourName] ..the diff..
      ```
    - in `runner/index.ts` inside `interfaceEditImplementation`, add a `case "Edit[YourName]" : return (yourInput) => { ... }`
-}


{-| The "model" in a [`Web.program`](#program)
-}
type ProgramState appState
    = State
        { interface : Interface appState
        , appState : appState
        }



{-

   The vast majority of InternalFastDict-related code is copied from [miniBill/elm-fast-dict](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest/)
   with below license



   Copyright 2023-present Leonardo Taglialegne
   Copyright 2014-present Evan Czaplicki

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}


{-| Alternative to `Dict` that internally allows fast creation from already sorted elements
which significantly speeds up DOM interface creation.
-}
type InternalFastDict key value
    = InternalFastDict Int (InternalFastDictInner key value)


{-| The color of an [`InternalFastDictInner`](#InternalFastDictInner) node. Leaves are considered Black.
-}
type InternalFastDictNColor
    = InternalFastDictRed
    | InternalFastDictBlack


{-| Structure of an [`InternalFastDict`](#InternalFastDict)
-}
type InternalFastDictInner key value
    = InternalFastDictInnerNode InternalFastDictNColor key value (InternalFastDictInner key value) (InternalFastDictInner key value)
    | InternalFastDictLeaf


{-| Create an empty dictionary.
-}
internalFastDictEmpty : InternalFastDict key_ value_
internalFastDictEmpty =
    InternalFastDict 0 InternalFastDictLeaf


internalFastDictBalance : InternalFastDictNColor -> k -> v -> InternalFastDictInner k v -> InternalFastDictInner k v -> InternalFastDictInner k v
internalFastDictBalance color key value left right =
    case right of
        InternalFastDictInnerNode InternalFastDictRed rK rV rLeft rRight ->
            case left of
                InternalFastDictInnerNode InternalFastDictRed lK lV lLeft lRight ->
                    InternalFastDictInnerNode
                        InternalFastDictRed
                        key
                        value
                        (InternalFastDictInnerNode InternalFastDictBlack lK lV lLeft lRight)
                        (InternalFastDictInnerNode InternalFastDictBlack rK rV rLeft rRight)

                _ ->
                    InternalFastDictInnerNode color rK rV (InternalFastDictInnerNode InternalFastDictRed key value left rLeft) rRight

        _ ->
            case left of
                InternalFastDictInnerNode InternalFastDictRed lK lV (InternalFastDictInnerNode InternalFastDictRed llK llV llLeft llRight) lRight ->
                    InternalFastDictInnerNode
                        InternalFastDictRed
                        lK
                        lV
                        (InternalFastDictInnerNode InternalFastDictBlack llK llV llLeft llRight)
                        (InternalFastDictInnerNode InternalFastDictBlack key value lRight right)

                _ ->
                    InternalFastDictInnerNode color key value left right


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
internalFastDictFoldl : (k -> v -> b -> b) -> b -> InternalFastDict k v -> b
internalFastDictFoldl func acc (InternalFastDict _ dict) =
    internalFastDictInnerFoldl func acc dict


internalFastDictInnerFoldl : (k -> v -> b -> b) -> b -> InternalFastDictInner k v -> b
internalFastDictInnerFoldl func acc dict =
    case dict of
        InternalFastDictLeaf ->
            acc

        InternalFastDictInnerNode _ key value left right ->
            internalFastDictInnerFoldl func (func key value (internalFastDictInnerFoldl func acc left)) right


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
To create one, use the helpers in [time](#time), [DOM](#dom), [HTTP](#http) etc.

To combine multiple, use [`Web.interfaceBatch`](#interfaceBatch) and [`Web.interfaceNone`](#interfaceNone).
To change the value that comes back in the future, use [`Web.interfaceFutureMap`](Web#interfaceFutureMap)

-}
type alias Interface future =
    InternalFastDict String (InterfaceSingle future)


{-| A "non-batched" [`Interface`](#Interface).
To create one, use the helpers in [time](#time), [DOM](#dom), [HTTP](#http) etc.
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
    | FileDownloadUnsignedInt8s { mimeType : String, name : String, content : List Int }
    | ClipboardReplaceBy String
    | ClipboardRequest (String -> future)
    | AudioSourceLoad { url : String, on : Result AudioSourceLoadError AudioSource -> future }
    | AudioPlay Audio
    | DomNodeRender
        { path :
            -- from outer parent to inner parent index
            List Int
        , node : DomTextOrElementHeader future
        }
    | NotificationAskForPermission ()
    | NotificationShow { id : String, message : String, details : String, on : NotificationClicked -> future }
    | HttpRequest (HttpRequest future)
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
    | SocketConnect { address : String, on : SocketConnectionEvent -> future }
    | SocketMessage { id : SocketId, data : String }
    | SocketDisconnect SocketId
    | SocketMessageListen { id : SocketId, on : String -> future }
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


{-| An indication that connection has changed
after having initiated [`Web.socketConnectTo`](Web#socketConnectTo).

  - `SocketConnected`: connection has been opened. Gives access to a [`Web.SocketId`](#SocketId)
  - `SocketDisconnected`: connection has been closed with
      - the close `code` sent by the server
      - The `reason` indicating why the server closed the connection, specific to the particular server and sub-protocol

-}
type SocketConnectionEvent
    = SocketConnected SocketId
    | SocketDisconnected { code : Int, reason : String }


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


{-| An HTTP request for use in an [`Interface`](#Interface).

Use [`Web.httpAddHeaders`](Web#httpAddHeaders) to set custom headers as needed.
Use [`Web.timeOnceAt`](Web#timeOnceAt) to add a timeout of how long you are willing to wait before giving up.

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

  - `HttpBodyString`: Put a `String` in the body of your request. Defining `Web.httpJsonBody` looks like this:

        import Json.Encode

        httpJsonBody : Json.Encode.Value -> Web.HttpBody
        httpJsonBody value =
            Web.HttpBodyString "application/json" (Json.Encode.encode 0 value)

    The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type) of the body.

  - `HttpBodyUnsignedInt8s` is pretty much the same as `HttpBodyString` but for [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/),
    see [`Web.httpBodyBytes`](Web#httpBodyBytes)

-}
type HttpBody
    = HttpBodyEmpty
    | HttpBodyString { mimeType : String, content : String }
    | HttpBodyUnsignedInt8s { mimeType : String, content : List Int }


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


{-| Local identifier for a [web socket](#socket) for this session
that can be used to [communicate](Web#communicate)
-}
type SocketId
    = SocketId Int


{-| Combine multiple [`Interface`](#Interface)s into one
-}
interfaceBatch : List (Interface future) -> Interface future
interfaceBatch interfaces =
    interfaces |> List.foldl internalFastDictUnion internalFastDictEmpty


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
internalFastDictUnion : InternalFastDict comparable v -> InternalFastDict comparable v -> InternalFastDict comparable v
internalFastDictUnion ((InternalFastDict s1 _) as t1) ((InternalFastDict s2 _) as t2) =
    if s1 > s2 then
        internalFastDictFoldl internalFastDictInsertNoReplace t1 t2

    else
        internalFastDictFoldl internalFastDictInsert t2 t1


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
internalFastDictInsert : comparable -> v -> InternalFastDict comparable v -> InternalFastDict comparable v
internalFastDictInsert key value (InternalFastDict sz dict) =
    let
        ( result, isNew ) =
            internalFastDictInnerInsert key value dict
    in
    if isNew then
        InternalFastDict (sz + 1) result

    else
        InternalFastDict sz result


internalFastDictInnerInsert : comparable -> v -> InternalFastDictInner comparable v -> ( InternalFastDictInner comparable v, Bool )
internalFastDictInnerInsert key value dict =
    -- Root node is always Black
    case internalFastDictInsertHelp key value dict of
        ( InternalFastDictInnerNode InternalFastDictRed k v l r, isNew ) ->
            ( InternalFastDictInnerNode InternalFastDictBlack k v l r, isNew )

        x ->
            x


internalFastDictInsertHelp : comparable -> v -> InternalFastDictInner comparable v -> ( InternalFastDictInner comparable v, Bool )
internalFastDictInsertHelp key value dict =
    case dict of
        InternalFastDictLeaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( InternalFastDictInnerNode InternalFastDictRed key value InternalFastDictLeaf InternalFastDictLeaf, True )

        InternalFastDictInnerNode nColor nKey nValue nLeft nRight ->
            case compare key nKey of
                LT ->
                    let
                        ( newLeft, isNew ) =
                            internalFastDictInsertHelp key value nLeft
                    in
                    ( internalFastDictBalance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( InternalFastDictInnerNode nColor nKey value nLeft nRight, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            internalFastDictInsertHelp key value nRight
                    in
                    ( internalFastDictBalance nColor nKey nValue nLeft newRight, isNew )


internalFastDictInsertNoReplace : comparable -> v -> InternalFastDict comparable v -> InternalFastDict comparable v
internalFastDictInsertNoReplace key value (InternalFastDict sz dict) =
    let
        ( result, isNew ) =
            internalFastDictInnerInsertNoReplace key value dict
    in
    if isNew then
        InternalFastDict (sz + 1) result

    else
        InternalFastDict sz result


internalFastDictInnerInsertNoReplace : comparable -> v -> InternalFastDictInner comparable v -> ( InternalFastDictInner comparable v, Bool )
internalFastDictInnerInsertNoReplace key value dict =
    -- Root node is always Black
    case internalFastDictInsertHelpNoReplace key value dict of
        ( InternalFastDictInnerNode InternalFastDictRed k v l r, isNew ) ->
            ( InternalFastDictInnerNode InternalFastDictBlack k v l r, isNew )

        x ->
            x


internalFastDictInsertHelpNoReplace : comparable -> v -> InternalFastDictInner comparable v -> ( InternalFastDictInner comparable v, Bool )
internalFastDictInsertHelpNoReplace key value dict =
    case dict of
        InternalFastDictLeaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( InternalFastDictInnerNode InternalFastDictRed key value InternalFastDictLeaf InternalFastDictLeaf, True )

        InternalFastDictInnerNode nColor nKey nValue nLeft nRight ->
            case compare key nKey of
                LT ->
                    let
                        ( newLeft, isNew ) =
                            internalFastDictInsertHelpNoReplace key value nLeft
                    in
                    ( internalFastDictBalance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( dict, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            internalFastDictInsertHelpNoReplace key value nRight
                    in
                    ( internalFastDictBalance nColor nKey nValue nLeft newRight, isNew )


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
    internalFastDictEmpty


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
interfaceFutureMap : (future -> mappedFuture) -> (Interface future -> Interface mappedFuture)
interfaceFutureMap futureChange interface =
    interface
        |> internalFastDictMap
            (\interfaceSingle ->
                interfaceSingle |> interfaceSingleFutureMap futureChange
            )


internalFastDictMap : (a -> b) -> InternalFastDict k a -> InternalFastDict k b
internalFastDictMap func (InternalFastDict sz dict) =
    InternalFastDict sz (internalFastDictInnerMap func dict)


internalFastDictInnerMap : (a -> b) -> InternalFastDictInner k a -> InternalFastDictInner k b
internalFastDictInnerMap func dict =
    case dict of
        InternalFastDictLeaf ->
            InternalFastDictLeaf

        InternalFastDictInnerNode color key value left right ->
            InternalFastDictInnerNode color key (func value) (internalFastDictInnerMap func left) (internalFastDictInnerMap func right)


interfaceSingleFutureMap : (future -> mappedFuture) -> (InterfaceSingle future -> InterfaceSingle mappedFuture)
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

        FileDownloadUnsignedInt8s download ->
            FileDownloadUnsignedInt8s download

        ClipboardReplaceBy clipboard ->
            ClipboardReplaceBy clipboard

        AudioPlay audio ->
            AudioPlay audio

        SocketMessage message ->
            SocketMessage message

        SocketDisconnect id ->
            SocketDisconnect id

        LocalStorageSet localStorageItem ->
            LocalStorageSet localStorageItem

        NotificationAskForPermission () ->
            notificationAskForPermissionSingle

        DomNodeRender toRender ->
            { path = toRender.path
            , node = toRender.node |> domNodeFutureMap futureChange
            }
                |> DomNodeRender

        AudioSourceLoad sourceLoad ->
            { url = sourceLoad.url, on = \event -> sourceLoad.on event |> futureChange }
                |> AudioSourceLoad

        SocketConnect connect ->
            { address = connect.address, on = \event -> event |> connect.on |> futureChange }
                |> SocketConnect

        NotificationShow show ->
            { id = show.id
            , message = show.message
            , details = show.details
            , on = \future -> future |> show.on |> futureChange
            }
                |> NotificationShow

        HttpRequest request ->
            request |> httpRequestFutureMap futureChange |> HttpRequest

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

        RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
            { count = randomUnsignedInt32sRequest.count
            , on = \ints -> randomUnsignedInt32sRequest.on ints |> futureChange
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

        TimePeriodicallyListen periodicallyListen ->
            { intervalDurationMilliSeconds = periodicallyListen.intervalDurationMilliSeconds
            , on = \posix -> periodicallyListen.on posix |> futureChange
            }
                |> TimePeriodicallyListen

        DocumentEventListen listen ->
            { eventName = listen.eventName, on = listen.on |> Json.Decode.map futureChange }
                |> DocumentEventListen

        SocketMessageListen messageListen ->
            { id = messageListen.id, on = \event -> event |> messageListen.on |> futureChange }
                |> SocketMessageListen

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


domNodeFutureMap : (future -> mappedFuture) -> (DomTextOrElementHeader future -> DomTextOrElementHeader mappedFuture)
domNodeFutureMap futureChange domElementToMap =
    case domElementToMap of
        DomHeaderText text ->
            DomHeaderText text

        DomElementHeader domElementHeader ->
            domElementHeader |> domElementHeaderFutureMap futureChange |> DomElementHeader


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


domElementHeaderFutureMap : (future -> mappedFuture) -> (DomElementHeader future -> DomElementHeader mappedFuture)
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
    -> (SortedKeyValueList key value -> SortedKeyValueList key newValue)
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
    | JsEventEnabledConstructionOfNewAppState appState


interfaceSingleEditsMap :
    (InterfaceSingleEdit -> fromSingeEdit)
    ->
        ({ old : InterfaceSingle future, updated : InterfaceSingle future }
         -> List fromSingeEdit
        )
interfaceSingleEditsMap fromSingeEdit interfaces =
    case interfaces.old of
        DomNodeRender domElementPreviouslyRendered ->
            case interfaces.updated of
                DomNodeRender domElementToRender ->
                    { old = domElementPreviouslyRendered.node, updated = domElementToRender.node }
                        |> domTextOrElementHeaderDiffMap
                            (\diff ->
                                { path = domElementPreviouslyRendered.path
                                , replacement = diff
                                }
                                    |> EditDom
                                    |> fromSingeEdit
                            )

                _ ->
                    []

        AudioPlay previouslyPlayed ->
            case interfaces.updated of
                AudioPlay toPlay ->
                    { old = previouslyPlayed, updated = toPlay }
                        |> audioDiffMap
                            (\diff ->
                                { url = toPlay.url, startTime = toPlay.startTime, replacement = diff }
                                    |> EditAudio
                                    |> fromSingeEdit
                            )

                _ ->
                    []

        NotificationShow _ ->
            case interfaces.updated of
                NotificationShow toShow ->
                    [ { id = toShow.id, message = toShow.message, details = toShow.details }
                        |> EditNotification
                        |> fromSingeEdit
                    ]

                _ ->
                    []

        DocumentTitleReplaceBy _ ->
            []

        DocumentAuthorSet _ ->
            []

        DocumentKeywordsSet _ ->
            []

        DocumentDescriptionSet _ ->
            []

        DocumentEventListen _ ->
            []

        ConsoleLog _ ->
            []

        ConsoleWarn _ ->
            []

        ConsoleError _ ->
            []

        NavigationReplaceUrl _ ->
            []

        NavigationPushUrl _ ->
            []

        NavigationGo _ ->
            []

        NavigationLoad _ ->
            []

        NavigationReload () ->
            []

        NavigationUrlRequest _ ->
            []

        FileDownloadUnsignedInt8s _ ->
            []

        ClipboardReplaceBy _ ->
            []

        ClipboardRequest _ ->
            []

        AudioSourceLoad _ ->
            []

        NotificationAskForPermission () ->
            []

        HttpRequest _ ->
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

        WindowSizeRequest _ ->
            []

        WindowPreferredLanguagesRequest _ ->
            []

        WindowEventListen _ ->
            []

        WindowVisibilityChangeListen _ ->
            []

        WindowAnimationFrameListen _ ->
            []

        WindowPreferredLanguagesChangeListen _ ->
            []

        SocketConnect _ ->
            []

        SocketMessage _ ->
            []

        SocketDisconnect _ ->
            []

        SocketMessageListen _ ->
            []

        LocalStorageSet _ ->
            []

        LocalStorageRequest _ ->
            []

        LocalStorageRemoveOnADifferentTabListen _ ->
            []

        LocalStorageSetOnADifferentTabListen _ ->
            []

        GeoLocationRequest _ ->
            []

        GeoLocationChangeListen _ ->
            []

        GamepadsRequest _ ->
            []

        GamepadsChangeListen _ ->
            []


domTextOrElementHeaderDiffMap :
    (DomEdit -> fromDomEdit)
    ->
        ({ old : DomTextOrElementHeader state, updated : DomTextOrElementHeader state }
         -> List fromDomEdit
        )
domTextOrElementHeaderDiffMap fromDomEdit nodes =
    case nodes.old of
        DomHeaderText oldText ->
            case nodes.updated of
                DomElementHeader updatedElement ->
                    [ updatedElement
                        |> domElementHeaderFutureMap (\_ -> ())
                        |> DomElementHeader
                        |> ReplacementDomNode
                        |> fromDomEdit
                    ]

                DomHeaderText updatedText ->
                    if oldText == updatedText ++ "" then
                        []

                    else
                        [ updatedText
                            |> DomHeaderText
                            |> ReplacementDomNode
                            |> fromDomEdit
                        ]

        DomElementHeader oldElement ->
            case nodes.updated of
                DomHeaderText updatedText ->
                    [ updatedText
                        |> DomHeaderText
                        |> ReplacementDomNode
                        |> fromDomEdit
                    ]

                DomElementHeader updatedElement ->
                    { old = oldElement, updated = updatedElement }
                        |> domElementHeaderDiffMap fromDomEdit


domElementHeaderDiffMap :
    (DomEdit -> fromDomEdit)
    ->
        ({ old : DomElementHeader future, updated : DomElementHeader future }
         -> List fromDomEdit
        )
domElementHeaderDiffMap fromDomEdit elements =
    if elements.old.tag /= elements.updated.tag then
        [ elements.updated
            |> domElementHeaderFutureMap (\_ -> ())
            |> DomElementHeader
            |> ReplacementDomNode
            |> fromDomEdit
        ]

    else
        { old = elements.old.styles, updated = elements.updated.styles }
            |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                (\d -> d |> ReplacementDomElementStyles |> fromDomEdit)
                { remove = identity, edit = Basics.identity }
            |> List.LocalExtra.fromMaybe
            |> List.LocalExtra.consJust
                ({ old = elements.old.attributes, updated = elements.updated.attributes }
                    |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                        (\d -> d |> ReplacementDomElementAttributes |> fromDomEdit)
                        { remove = identity, edit = Basics.identity }
                )
            |> List.LocalExtra.consJust
                ({ old = elements.old.attributesNamespaced, updated = elements.updated.attributesNamespaced }
                    |> sortedKeyValueListEditAndRemoveDiffMapBy namespacedKeyToComparable
                        (\d -> d |> ReplacementDomElementAttributesNamespaced |> fromDomEdit)
                        { remove = \k -> { namespace = k.namespace, key = k.key }
                        , edit = \entry -> { namespace = entry.key.namespace, key = entry.key.key, value = entry.value }
                        }
                )
            |> List.LocalExtra.consJust
                ({ old = elements.old.stringProperties, updated = elements.updated.stringProperties }
                    |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                        (\d -> d |> ReplacementDomElementStringProperties |> fromDomEdit)
                        { remove = identity, edit = Basics.identity }
                )
            |> List.LocalExtra.consJust
                ({ old = elements.old.boolProperties, updated = elements.updated.boolProperties }
                    |> sortedKeyValueListEditAndRemoveDiffMapBy Basics.identity
                        (\d -> d |> ReplacementDomElementBoolProperties |> fromDomEdit)
                        { remove = identity, edit = Basics.identity }
                )
            |> List.LocalExtra.consJust
                (if elements.old.scrollToPosition == elements.updated.scrollToPosition then
                    Nothing

                 else
                    ReplacementDomElementScrollToPosition elements.updated.scrollToPosition
                        |> fromDomEdit
                        |> Just
                )
            |> List.LocalExtra.consJust
                (if elements.old.scrollToShow == elements.updated.scrollToShow then
                    Nothing

                 else
                    ReplacementDomElementScrollToShow elements.updated.scrollToShow
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
                    ReplacementDomElementEventListens updatedElementEventListensId
                        |> fromDomEdit
                        |> Just
                )


replacementDomElementScrollPositionRequest : DomEdit
replacementDomElementScrollPositionRequest =
    ReplacementDomElementScrollPositionRequest ()


sortedKeyValueListEditAndRemoveDiffMapBy :
    (key -> comparable_)
    -> ({ remove : List removeSingle, edit : List editSingle } -> fromRemoveAndEdit)
    -> { remove : key -> removeSingle, edit : { key : key, value : value } -> editSingle }
    ->
        ({ old : SortedKeyValueList key value
         , updated : SortedKeyValueList key value
         }
         -> Maybe fromRemoveAndEdit
        )
sortedKeyValueListEditAndRemoveDiffMapBy keyToComparable fromEditAndRemove asDiffSingle dicts =
    let
        diff : { remove : List removeSingle, edit : List editSingle }
        diff =
            sortedKeyValueListMergeBy keyToComparable
                (\removed soFar ->
                    { edit = soFar.edit
                    , remove = asDiffSingle.remove removed.key :: soFar.remove
                    }
                )
                (\old updated soFar ->
                    if old == updated.value then
                        soFar

                    else
                        { remove = soFar.remove
                        , edit = asDiffSingle.edit updated :: soFar.edit
                        }
                )
                (\updated soFar ->
                    { remove = soFar.remove
                    , edit = asDiffSingle.edit updated :: soFar.edit
                    }
                )
                (dicts.old |> sortedKeyValueListToList)
                (dicts.updated |> sortedKeyValueListToList)
                { remove = [], edit = [] }
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


audioDiffMap :
    (AudioEdit -> fromAudioEdit)
    -> ({ old : Audio, updated : Audio } -> List fromAudioEdit)
audioDiffMap fromAudioEdit audios =
    (if audios.old.volume == audios.updated.volume then
        []

     else
        [ ReplacementAudioVolume audios.updated.volume |> fromAudioEdit ]
    )
        |> List.LocalExtra.consJust
            (if audios.old.speed == audios.updated.speed then
                Nothing

             else
                ReplacementAudioSpeed audios.updated.speed |> fromAudioEdit |> Just
            )
        |> List.LocalExtra.consJust
            (if audios.old.stereoPan == audios.updated.stereoPan then
                Nothing

             else
                ReplacementAudioStereoPan audios.updated.stereoPan |> fromAudioEdit |> Just
            )
        |> List.LocalExtra.consJust
            (if audios.old.processingLastToFirst == audios.updated.processingLastToFirst then
                Nothing

             else
                audios.updated.processingLastToFirst
                    |> List.reverse
                    |> ReplacementAudioProcessing
                    |> fromAudioEdit
                    |> Just
            )


namespacedKeyToComparable : { namespace : String, key : String } -> String
namespacedKeyToComparable =
    \namespacedKey -> namespacedKey.key ++ String.cons ' ' namespacedKey.namespace


{-| Alternative to `Dict` optimized for fast merge and fast creation.
Would be a terrible fit if we needed fast insert and get.
-}
type SortedKeyValueList key value
    = SortedKeyValueList (List { key : key, value : value })


sortedKeyValueListToList : SortedKeyValueList key value -> List { key : key, value : value }
sortedKeyValueListToList (SortedKeyValueList sortedKeyValueList) =
    sortedKeyValueList


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
            EditDom editDomDiff ->
                { tag = "EditDom"
                , value =
                    Json.Encode.object
                        [ ( "path", editDomDiff.path |> Json.Encode.list Json.Encode.int )
                        , ( "replacement", editDomDiff.replacement |> editDomDiffToJson )
                        ]
                }

            EditAudio audioEdit ->
                { tag = "EditAudio"
                , value =
                    Json.Encode.object
                        [ ( "url", audioEdit.url |> Json.Encode.string )
                        , ( "startTime", audioEdit.startTime |> Time.posixToMillis |> Json.Encode.int )
                        , ( "replacement"
                          , Json.Encode.LocalExtra.variant
                                (case audioEdit.replacement of
                                    ReplacementAudioSpeed new ->
                                        { tag = "Speed", value = new |> audioParameterTimelineToJson }

                                    ReplacementAudioVolume new ->
                                        { tag = "Volume", value = new |> audioParameterTimelineToJson }

                                    ReplacementAudioStereoPan new ->
                                        { tag = "StereoPan", value = new |> audioParameterTimelineToJson }

                                    ReplacementAudioProcessing new ->
                                        { tag = "Processing"
                                        , value = new |> Json.Encode.list audioProcessingToJson
                                        }
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


editDomDiffToJson : DomEdit -> Json.Encode.Value
editDomDiffToJson =
    \replacementInEditDomDiff ->
        Json.Encode.LocalExtra.variant
            (case replacementInEditDomDiff of
                ReplacementDomNode node ->
                    { tag = "Node", value = node |> domTextOrElementHeaderInfoToJson }

                ReplacementDomElementStyles styles ->
                    { tag = "Styles"
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

                ReplacementDomElementAttributes attributes ->
                    { tag = "Attributes"
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

                ReplacementDomElementAttributesNamespaced attributesNamespaced ->
                    { tag = "AttributesNamespaced"
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

                ReplacementDomElementStringProperties stringProperties ->
                    { tag = "StringProperties"
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

                ReplacementDomElementBoolProperties boolProperties ->
                    { tag = "StringProperties"
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

                ReplacementDomElementScrollToPosition maybePosition ->
                    { tag = "ScrollToPosition"
                    , value = maybePosition |> Json.Encode.LocalExtra.nullable domElementScrollPositionToJson
                    }

                ReplacementDomElementScrollToShow alignment ->
                    { tag = "ScrollToShow"
                    , value = alignment |> Json.Encode.LocalExtra.nullable domElementVisibilityAlignmentsToJson
                    }

                ReplacementDomElementScrollPositionRequest () ->
                    { tag = "ScrollPositionRequest", value = Json.Encode.null }

                ReplacementDomElementEventListens listens ->
                    { tag = "EventListens"
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
interfaceSingleToJson =
    \interfaceSingle ->
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

                FileDownloadUnsignedInt8s config ->
                    { tag = "FileDownloadUnsignedInt8s"
                    , value =
                        Json.Encode.object
                            [ ( "name", config.name |> Json.Encode.string )
                            , ( "mimeType", config.mimeType |> Json.Encode.string )
                            , ( "content"
                              , config.content |> Json.Encode.list Json.Encode.int
                              )
                            ]
                    }

                ClipboardReplaceBy replacement ->
                    { tag = "ClipboardReplaceBy"
                    , value = replacement |> Json.Encode.string
                    }

                AudioPlay audio ->
                    { tag = "AudioPlay", value = audio |> audioToJson }

                SocketMessage message ->
                    { tag = "SocketMessage"
                    , value =
                        Json.Encode.object
                            [ ( "id", message.id |> socketIdToJson )
                            , ( "data", message.data |> Json.Encode.string )
                            ]
                    }

                SocketDisconnect id ->
                    { tag = "SocketDisconnect"
                    , value = id |> socketIdToJson
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

                DomNodeRender render ->
                    { tag = "DomNodeRender"
                    , value =
                        Json.Encode.object
                            [ ( "path", render.path |> Json.Encode.list Json.Encode.int )
                            , ( "node", render.node |> domTextOrElementHeaderInfoToJson )
                            ]
                    }

                AudioSourceLoad sourceLoad ->
                    { tag = "AudioSourceLoad", value = sourceLoad.url |> Json.Encode.string }

                SocketConnect connect ->
                    { tag = "SocketConnect"
                    , value = Json.Encode.object [ ( "address", connect.address |> Json.Encode.string ) ]
                    }

                NotificationShow show ->
                    { tag = "NotificationShow"
                    , value =
                        Json.Encode.object
                            [ ( "id", show.id |> Json.Encode.string )
                            , ( "message", show.message |> Json.Encode.string )
                            , ( "details", show.details |> Json.Encode.string )
                            ]
                    }

                HttpRequest httpRequestInfo ->
                    { tag = "HttpRequest", value = httpRequestInfo |> httpRequestInfoToJson }

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

                DocumentEventListen listen ->
                    { tag = "DocumentEventListen", value = listen.eventName |> Json.Encode.string }

                SocketMessageListen listen ->
                    { tag = "SocketMessageListen", value = listen.id |> socketIdToJson }

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


socketIdToJson : SocketId -> Json.Encode.Value
socketIdToJson =
    \(SocketId index) -> index |> Json.Encode.int


httpRequestInfoToJson : HttpRequest future_ -> Json.Encode.Value
httpRequestInfoToJson =
    \httpRequestId ->
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
httpExpectInfoToJson =
    \httpExpectId ->
        Json.Encode.string
            (case httpExpectId of
                HttpExpectString _ ->
                    "String"

                HttpExpectBytes _ ->
                    "Bytes"

                HttpExpectWhatever _ ->
                    "Whatever"
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


domTextOrElementHeaderInfoToJson : DomTextOrElementHeader future_ -> Json.Encode.Value
domTextOrElementHeaderInfoToJson domNodeId =
    Json.Encode.LocalExtra.variant
        (case domNodeId of
            DomHeaderText text ->
                { tag = "Text", value = text |> Json.Encode.string }

            DomElementHeader element ->
                { tag = "Element", value = element |> domElementHeaderInfoToJson }
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
domElementBoolPropertiesToJson =
    \boolProperties ->
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
domElementStringPropertiesToJson =
    \stringProperties ->
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
defaultActionHandlingToJson =
    \defaultActionHandling ->
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
interfaceSingleToStructuredId =
    \interfaceSingle ->
        StructuredId.ofVariant
            (case interfaceSingle of
                DocumentTitleReplaceBy _ ->
                    { tag = "DocumentTitleReplaceBy", value = StructuredId.ofUnit }

                DocumentAuthorSet _ ->
                    { tag = "DocumentAuthorSet", value = StructuredId.ofUnit }

                DocumentKeywordsSet _ ->
                    { tag = "DocumentKeywordsSet"
                    , value = StructuredId.ofUnit
                    }

                DocumentDescriptionSet _ ->
                    { tag = "DocumentDescriptionSet", value = StructuredId.ofUnit }

                ConsoleLog message ->
                    { tag = "ConsoleLog", value = message |> StructuredId.ofString }

                ConsoleWarn message ->
                    { tag = "ConsoleWarn", value = message |> StructuredId.ofString }

                ConsoleError message ->
                    { tag = "ConsoleError", value = message |> StructuredId.ofString }

                NavigationReplaceUrl _ ->
                    { tag = "NavigationReplaceUrl", value = StructuredId.ofUnit }

                NavigationPushUrl _ ->
                    { tag = "NavigationPushUrl", value = StructuredId.ofUnit }

                NavigationGo urlSteps ->
                    { tag = "NavigationGo", value = urlSteps |> StructuredId.ofInt }

                NavigationLoad _ ->
                    { tag = "NavigationLoad", value = StructuredId.ofUnit }

                NavigationReload () ->
                    { tag = "NavigationReload", value = StructuredId.ofUnit }

                FileDownloadUnsignedInt8s config ->
                    { tag = "FileDownloadUnsignedInt8s"
                    , value =
                        StructuredId.ofParts
                            [ config.name |> StructuredId.ofString
                            , config.mimeType |> StructuredId.ofString
                            ]
                    }

                ClipboardReplaceBy _ ->
                    { tag = "ClipboardReplaceBy", value = StructuredId.ofUnit }

                AudioPlay audio ->
                    { tag = "AudioPlay"
                    , value =
                        StructuredId.ofParts
                            [ audio.url |> StructuredId.ofString
                            , audio.startTime |> Time.LocalExtra.posixToStructureId
                            ]
                    }

                SocketMessage message ->
                    { tag = "SocketMessage"
                    , value =
                        StructuredId.ofParts
                            [ message.id |> socketIdToStructuredId
                            , message.data |> StructuredId.ofString
                            ]
                    }

                SocketDisconnect id ->
                    { tag = "SocketDisconnect", value = id |> socketIdToStructuredId }

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

                DomNodeRender render ->
                    { tag = "DomNodeRender"
                    , value =
                        render.path |> indexListToDigitCountPrefixedStructureId
                    }

                AudioSourceLoad sourceLoad ->
                    { tag = "AudioSourceLoad"
                    , value = sourceLoad.url |> StructuredId.ofString
                    }

                SocketConnect connect ->
                    { tag = "SocketConnect"
                    , value = StructuredId.ofString connect.address
                    }

                NotificationShow show ->
                    { tag = "NotificationShow"
                    , value = show.id |> StructuredId.ofString
                    }

                HttpRequest request ->
                    { tag = "HttpRequest"
                    , value = request.url |> StructuredId.ofString
                    }

                TimePosixRequest _ ->
                    { tag = "TimePosixRequest", value = StructuredId.ofUnit }

                TimezoneOffsetRequest _ ->
                    { tag = "TimezoneOffsetRequest", value = StructuredId.ofUnit }

                TimezoneNameRequest _ ->
                    { tag = "TimezoneNameRequest", value = StructuredId.ofUnit }

                TimeOnce once ->
                    { tag = "TimeOnce", value = once.pointInTime |> Time.LocalExtra.posixToStructureId }

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

                SocketMessageListen listen ->
                    { tag = "SocketMessageListen"
                    , value = listen.id |> socketIdToStructuredId
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


{-| See indexToDigitCountPrefixed.
Orders index lists more nicely so that e.g. [] < [0] < [0,0].

The problem with StructuredId.ofList is: that with e.g. `"[]"` vs `"[0]"` any digit is < `']'`.
So we instead represent it as `"\"\""` vs `"\"0\""` because any digit is > `'"'`

-}
indexListToDigitCountPrefixedStructureId : List Int -> StructuredId
indexListToDigitCountPrefixedStructureId indexes =
    StructuredId.ofString
        (indexes
            |> commaSeparatedMap (\index -> index |> indexToDigitCountPrefixed |> String.fromInt)
        )


{-| Crazy idea this one!

We all know the problem that sorting strings
will sort "20" and "110" as `["110","20"]` which means
indexes do not get sorted in the order we create them.

To fix this, we could pad the numbers with 0s left:
"020" and "110" will be sorted as `["020","110"]`.
This however only works if we know how much to pad.
For the DOM, this would be roughly padding up to 5 digits.

A nicer (shorter and less max-digit-strict)
approach is to first prepend the number of digits to pre-sort based on that
and only then follow up with the precise number
which can now be correctly compared digit by digit since the digit count is the same:
"220" and "3110" will be sorted as `["220","3110"]`.
(This approach only works because we can assume that indexes are < 999999999)

---

Why make this whole roundabout encoding?
A DOM Interface is represented as a bunch of headers for all it's sub-paths.
Instead of inserting the pieces one-by-one into an interface dictionary,
we have a fast "from sorted list" constructor.
That one needs already sorted entries!

-}
indexToDigitCountPrefixed : Int -> Int
indexToDigitCountPrefixed int =
    if int <= 9 then
        int

    else
        let
            digitCount : Int
            digitCount =
                (int |> Basics.toFloat) |> Basics.logBase 10 |> Basics.ceiling
        in
        digitCount * 10 ^ digitCount + int


commaSeparatedMap : (a -> String) -> List a -> String
commaSeparatedMap elementToString list =
    case list of
        [] ->
            ""

        element0 :: element1Up ->
            elementToString element0
                ++ commaPrefixedMap elementToString element1Up


commaPrefixedMap : (a -> String) -> List a -> String
commaPrefixedMap elementToString list =
    case list of
        [] ->
            ""

        element0 :: element1Up ->
            ","
                ++ elementToString element0
                ++ commaPrefixedMap elementToString element1Up


socketIdToStructuredId : SocketId -> StructuredId
socketIdToStructuredId =
    \(SocketId raw) -> raw |> StructuredId.ofInt


{-| Sort a given list of { key, value } elements to create a [`SortedKeyValueList`](#SortedKeyValueList)
-}
sortedKeyValueListFromList : List { value : value, key : comparable } -> SortedKeyValueList comparable value
sortedKeyValueListFromList =
    \unsortedList ->
        SortedKeyValueList (unsortedList |> List.sortBy .key)


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
        |> internalFastDictFoldl
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
                                case state.interface |> internalFastDictGet originalInterfaceId of
                                    Just interfaceSingleAcceptingFuture ->
                                        case interfaceSingleAcceptingFuture |> interfaceSingleFutureJsonDecoder of
                                            Just eventDataDecoder ->
                                                Json.Decode.field "eventData" eventDataDecoder

                                            Nothing ->
                                                "interface did not expect any events" |> Json.Decode.fail

                                    Nothing ->
                                        "no associated interface found among ids\n"
                                            ++ (state.interface
                                                    |> internalFastDictToList
                                                    |> List.map Tuple.first
                                                    |> String.join "\n"
                                               )
                                            |> Json.Decode.fail
                            )
                        |> Json.Decode.map JsEventEnabledConstructionOfNewAppState
                    )
                |> Result.LocalExtra.valueOrOnError JsEventFailedToDecode
        )


internalFastDictGet : comparable -> InternalFastDict comparable value -> Maybe value
internalFastDictGet targetKey (InternalFastDict _ dict) =
    getInner targetKey dict


getInner : comparable -> InternalFastDictInner comparable v -> Maybe v
getInner targetKey dict =
    case dict of
        InternalFastDictLeaf ->
            Nothing

        InternalFastDictInnerNode _ key value left right ->
            case compare targetKey key of
                LT ->
                    getInner targetKey left

                EQ ->
                    Just value

                GT ->
                    getInner targetKey right


{-| [json `Decoder`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Decoder)
for the transformed event data coming back
-}
interfaceSingleFutureJsonDecoder : InterfaceSingle future -> Maybe (Json.Decode.Decoder future)
interfaceSingleFutureJsonDecoder interface =
    case interface of
        DocumentTitleReplaceBy _ ->
            Nothing

        DocumentAuthorSet _ ->
            Nothing

        DocumentKeywordsSet _ ->
            Nothing

        DocumentDescriptionSet _ ->
            Nothing

        ConsoleLog _ ->
            Nothing

        ConsoleWarn _ ->
            Nothing

        ConsoleError _ ->
            Nothing

        DocumentEventListen listen ->
            listen.on |> Just

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

        NavigationUrlRequest toFuture ->
            Url.LocalExtra.jsonDecoder
                |> Json.Decode.map AppUrl.fromUrl
                |> Json.Decode.map toFuture
                |> Just

        FileDownloadUnsignedInt8s _ ->
            Nothing

        ClipboardReplaceBy _ ->
            Nothing

        ClipboardRequest toFuture ->
            Json.Decode.string |> Json.Decode.map toFuture |> Just

        AudioSourceLoad sourceLoad ->
            Json.Decode.oneOf
                [ Json.Decode.map (\duration -> Ok { url = sourceLoad.url, duration = duration })
                    (Json.Decode.LocalExtra.variant "Success"
                        (Json.Decode.field "durationInSeconds"
                            (Json.Decode.map Duration.seconds Json.Decode.float)
                        )
                    )
                , Json.Decode.LocalExtra.variant "Error"
                    (Json.Decode.map Err audioSourceLoadErrorJsonDecoder)
                ]
                |> Json.Decode.map sourceLoad.on
                |> Just

        AudioPlay _ ->
            Nothing

        DomNodeRender toRender ->
            case toRender.node of
                DomHeaderText _ ->
                    Nothing

                DomElementHeader element ->
                    let
                        eventListenDecoder : Json.Decode.Decoder future
                        eventListenDecoder =
                            Json.Decode.LocalExtra.variant "EventListen"
                                (Json.Decode.map2 (\eventListen event -> eventListen.on event)
                                    (Json.Decode.field "name"
                                        (Json.Decode.string
                                            |> Json.Decode.andThen
                                                (\specificEventName ->
                                                    case element.eventListens |> sortedKeyValueListGetAtStringKey specificEventName of
                                                        Nothing ->
                                                            Json.Decode.fail "received event of a kind that isn't listened for"

                                                        Just eventListen ->
                                                            eventListen |> Json.Decode.succeed
                                                )
                                        )
                                    )
                                    (Json.Decode.field "event" Json.Decode.value)
                                )
                    in
                    (case element.scrollPositionRequest of
                        Nothing ->
                            eventListenDecoder

                        Just request ->
                            Json.Decode.oneOf
                                [ eventListenDecoder
                                , Json.Decode.LocalExtra.variant "ScrollPositionRequest"
                                    domElementScrollPositionJsonDecoder
                                    |> Json.Decode.map request
                                ]
                    )
                        |> Just

        NotificationAskForPermission () ->
            Nothing

        NotificationShow show ->
            notificationResponseJsonDecoder
                |> Json.Decode.map show.on
                |> Just

        HttpRequest request ->
            Json.Decode.oneOf
                [ Json.Decode.LocalExtra.variant "Success" (httpSuccessResponseJsonDecoder request.expect)
                , Json.Decode.LocalExtra.variant "Error" httpErrorJsonDecoder
                    |> Json.Decode.map (httpExpectOnError request.expect)
                ]
                |> Just

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

        RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
            Json.Decode.list Json.Decode.int
                |> Json.Decode.map randomUnsignedInt32sRequest.on
                |> Just

        WindowSizeRequest toFuture ->
            Json.Decode.map2 (\width height -> { width = width, height = height })
                (Json.Decode.field "width" Json.Decode.int)
                (Json.Decode.field "height" Json.Decode.int)
                |> Json.Decode.map toFuture
                |> Just

        WindowPreferredLanguagesRequest toFuture ->
            Json.Decode.list Json.Decode.string
                |> Json.Decode.map toFuture
                |> Just

        WindowEventListen listen ->
            listen.on |> Just

        WindowVisibilityChangeListen toFuture ->
            windowVisibilityJsonDecoder |> Json.Decode.map toFuture |> Just

        WindowAnimationFrameListen toFuture ->
            Time.LocalExtra.posixJsonDecoder |> Json.Decode.map toFuture |> Just

        WindowPreferredLanguagesChangeListen toFuture ->
            Json.Decode.list Json.Decode.string
                |> Json.Decode.map toFuture
                |> Just

        SocketConnect connect ->
            socketConnectionEventJsonDecoder
                |> Json.Decode.map connect.on
                |> Just

        SocketMessage _ ->
            Nothing

        SocketDisconnect _ ->
            Nothing

        SocketMessageListen messageListen ->
            Json.Decode.string |> Json.Decode.map messageListen.on |> Just

        LocalStorageSet _ ->
            Nothing

        LocalStorageRequest request ->
            Json.Decode.nullable Json.Decode.string
                |> Json.Decode.map request.on
                |> Just

        LocalStorageRemoveOnADifferentTabListen listen ->
            Url.LocalExtra.jsonDecoder
                |> Json.Decode.map AppUrl.fromUrl
                |> Json.Decode.map listen.on
                |> Just

        LocalStorageSetOnADifferentTabListen listen ->
            Json.Decode.map3
                (\appUrl oldValue newValue ->
                    { appUrl = appUrl, oldValue = oldValue, newValue = newValue }
                )
                (Json.Decode.field "url"
                    (Url.LocalExtra.jsonDecoder |> Json.Decode.map AppUrl.fromUrl)
                )
                (Json.Decode.field "oldValue" (Json.Decode.nullable Json.Decode.string))
                (Json.Decode.field "newValue" Json.Decode.string)
                |> Json.Decode.map listen.on
                |> Just

        GeoLocationRequest toFuture ->
            geoLocationJsonDecoder |> Json.Decode.map toFuture |> Just

        GeoLocationChangeListen toFuture ->
            geoLocationJsonDecoder |> Json.Decode.map toFuture |> Just

        GamepadsRequest toFuture ->
            gamepadsJsonDecoder |> Json.Decode.map toFuture |> Just

        GamepadsChangeListen toFuture ->
            gamepadsJsonDecoder |> Json.Decode.map toFuture |> Just


{-| The fact that this can only be implemented linearly might seem shocking.
In reality, merging and creating a FastDict.Dict that gets thrown away after the next .get is way heavier (that's the theory at least).
-}
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


domElementScrollPositionJsonDecoder : Json.Decode.Decoder { fromLeft : Float, fromTop : Float }
domElementScrollPositionJsonDecoder =
    Json.Decode.map2 (\fromLeft fromTop -> { fromLeft = fromLeft, fromTop = fromTop })
        (Json.Decode.field "fromLeft" Json.Decode.float)
        (Json.Decode.field "fromTop" Json.Decode.float)


notificationResponseJsonDecoder : Json.Decode.Decoder NotificationClicked
notificationResponseJsonDecoder =
    Json.Decode.map (\() -> NotificationClicked) (Json.Decode.LocalExtra.onlyString "Clicked")


socketConnectionEventJsonDecoder : Json.Decode.Decoder SocketConnectionEvent
socketConnectionEventJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map SocketConnected (Json.Decode.LocalExtra.variant "SocketConnected" socketIdJsonDecoder)
        , Json.Decode.map SocketDisconnected
            (Json.Decode.LocalExtra.variant "SocketDisconnected"
                (Json.Decode.map2 (\code reason -> { code = code, reason = reason })
                    (Json.Decode.field "code" Json.Decode.int)
                    (Json.Decode.field "reason" Json.Decode.string)
                )
            )
        ]


socketIdJsonDecoder : Json.Decode.Decoder SocketId
socketIdJsonDecoder =
    Json.Decode.map SocketId Json.Decode.int


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
                    if not connected then
                        Nothing |> Json.Decode.succeed

                    else
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
                                    |> Just
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
                )
        )
        |> Json.Decode.map (Maybe.andThen identity)


gamepadButtonUnknown : GamepadButton
gamepadButtonUnknown =
    GamepadButtonReleased { isTouched = False }


gamepadThumbstickUnknown : { x : Float, y : Float }
gamepadThumbstickUnknown =
    { x = 0, y = 0 }


gamepadThumbsticksFromAxes : List Float -> List { x : Float, y : Float }
gamepadThumbsticksFromAxes axes =
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


listPadToAtLeast : Int -> a -> (List a -> List a)
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

            -- these are only analog
            , leftTrigger = Nothing
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
        -- nintendo style
        |> fastDictInsertSameValueFor
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
        -- dualsense style
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
        |> fastDictInsertSameValueFor
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
        -- playstation 4 style
        |> fastDictInsertSameValueFor
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
        -- logi style
        |> fastDictInsertSameValueFor
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
    -> (FastDict.Dict comparableKey value -> FastDict.Dict comparableKey value)
fastDictInsertSameValueFor keyList value dict =
    keyList
        |> List.foldl
            (\key dictSoFar -> dictSoFar |> FastDict.insert key value)
            dict


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
                        |> sortedKeyValueListFromList
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
                     , ". Please open an issue on github.com/lue-bird/elm-state-interface-experimental"
                     ]
                        |> String.concat
                    )
                        |> ConsoleError
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
    internalFastDictMerge
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


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
internalFastDictMerge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> InternalFastDict comparable a
    -> InternalFastDict comparable b
    -> result
    -> result
internalFastDictMerge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState : comparable -> b -> ( List ( comparable, a ), result ) -> ( List ( comparable, a ), result )
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( [], rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )

                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )

                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            internalFastDictFoldl stepState ( internalFastDictToList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers


remove : InterfaceSingleDiff irrelevantFuture_
remove =
    Remove ()


internalFastDictToList : InternalFastDict k v -> List ( k, v )
internalFastDictToList dict =
    internalFastDictFoldr (\key value list -> ( key, value ) :: list) [] dict


internalFastDictFoldr : (k -> v -> b -> b) -> b -> InternalFastDict k v -> b
internalFastDictFoldr func acc (InternalFastDict _ dict) =
    internalFastDictFoldrInner func acc dict


internalFastDictFoldrInner : (k -> v -> b -> b) -> b -> InternalFastDictInner k v -> b
internalFastDictFoldrInner func acc t =
    case t of
        InternalFastDictLeaf ->
            acc

        InternalFastDictInnerNode _ key value left right ->
            internalFastDictFoldrInner func (func key value (internalFastDictFoldrInner func acc right)) left


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
        , headers : SortedKeyValueList String String
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
    = EditDom { path : List Int, replacement : DomEdit }
    | EditAudio { url : String, startTime : Time.Posix, replacement : AudioEdit }
    | EditNotification { id : String, message : String, details : String }


{-| What parts of an [`Audio`](#Audio) are replaced
-}
type AudioEdit
    = ReplacementAudioVolume AudioParameterTimeline
    | ReplacementAudioSpeed AudioParameterTimeline
    | ReplacementAudioStereoPan AudioParameterTimeline
    | ReplacementAudioProcessing (List AudioProcessing)


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
type DomEdit
    = ReplacementDomNode (DomTextOrElementHeader ())
    | ReplacementDomElementStyles { edit : List { key : String, value : String }, remove : List String }
    | ReplacementDomElementAttributes { edit : List { key : String, value : String }, remove : List String }
    | ReplacementDomElementAttributesNamespaced { edit : List { namespace : String, key : String, value : String }, remove : List { namespace : String, key : String } }
    | ReplacementDomElementStringProperties { edit : List { key : String, value : String }, remove : List String }
    | ReplacementDomElementBoolProperties { edit : List { key : String, value : Bool }, remove : List String }
    | ReplacementDomElementScrollToPosition (Maybe { fromLeft : Float, fromTop : Float })
    | ReplacementDomElementScrollToShow (Maybe { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment })
    | ReplacementDomElementScrollPositionRequest ()
    | ReplacementDomElementEventListens (List { key : String, value : DefaultActionHandling })


{-| Create a [`Program`](#Program):

  - The state is everything the program knows (what The Elm Architecture calls model).
    And it always starts with a given `initialState`

  - The [`Interface`](#Interface) is the face to the outside world
    and can be created using the helpers in [time](#time), [DOM](#dom), [HTTP](#http) etc.
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
produced by [`Web.program`](#program)
-}
type alias Program state =
    Platform.Program () (ProgramState state) (ProgramEvent state)


interfaceFromSingle : InterfaceSingle future -> Interface future
interfaceFromSingle interfaceSingle =
    internalFastDictOne
        (interfaceSingle |> interfaceSingleToStructuredId |> StructuredId.toString)
        interfaceSingle


{-| Create a dictionary with one key-value pair.
-}
internalFastDictOne : key -> value -> InternalFastDict key value
internalFastDictOne key value =
    -- Root node is always Black
    InternalFastDict 1 (InternalFastDictInnerNode InternalFastDictBlack key value InternalFastDictLeaf InternalFastDictLeaf)


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


{-| Create an SVG element [`Web.DomNode`](Web#DomNode).
with a given tag, [`DomModifier`](Web#DomModifier)s and sub-nodes
-}
svgElement : String -> List (DomModifier future) -> List (DomNode future) -> DomNode future
svgElement tag modifiers subs =
    domElementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs


{-| An [`Interface`](Web#Interface) for opening a connection on a given address,
notifying you when it's [connected or disconnected](Web#SocketConnectionEvent)

Once this detects it's available, make sure to set your state's [`SocketId`](Web#SocketId) so you can actually [send](#socketMessage)
and [receive](#socketMessageListen) messages.
And once it's disconnected, set your state's [`SocketId`](Web#SocketId) back to nothing:

    case state.socketId of
        Nothing ->
            Web.socketConnectTo "ws://127.0.0.1:9000"
                |> Web.interfaceMap
                    (\connectionChanged ->
                        case connectionChanged of
                            Web.SocketConnected socketId ->
                                { state | socketId = socketId |> Just }

                            Web.SocketDisconnected ->
                                { state | socketId = Nothing }
                    )

        Just socketId ->
            Web.socketMessage socketId "Meow"

-}
socketConnectTo : String -> Interface SocketConnectionEvent
socketConnectTo address =
    SocketConnect { address = address, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for closing a given connection
-}
socketDisconnect : SocketId -> Interface future_
socketDisconnect id =
    SocketDisconnect id
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for sending data to the server.

It's common to pair this with [`Json.Encode.encode 0`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Encode#encode)
to send json.

-}
socketMessage : SocketId -> String -> Interface future_
socketMessage id data =
    SocketMessage { id = id, data = data }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for detecting when data has been sent from the server
-}
socketMessageListen : SocketId -> Interface String
socketMessageListen id =
    SocketMessageListen { id = id, on = identity }
        |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for generating a given count of cryptographically sound unsigned 32-bit `Int`s.
You can use these in all kinds of packages that allow creating an initial seed
from ints like [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/Random-Pcg-Extended#initialSeed)

Note: uses [`window.crypto.getRandomValues`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)

-}
randomUnsignedInt32s : Int -> Interface (List Int)
randomUnsignedInt32s count =
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


{-| Put a given JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
httpBodyJson : Json.Encode.Value -> HttpBody
httpBodyJson content =
    HttpBodyString { mimeType = "application/json", content = Json.Encode.encode 0 content }


{-| Put given [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/) in the body of your request.
The string argument should be a [MIME type](https://en.wikipedia.org/wiki/Media_type) to be used in the `Content-Type` header

    import Bytes exposing (Bytes)
    import Bytes.Encode
    import Time
    import Web
    import Zip
    import Zip.Entry

    exampleZipBody : Web.HttpBody
    exampleZipBody =
        Web.httpBodyBytes "application/zip"
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

  - `Err` with an [`HttpError`](Web#HttpError) if it didn't succeed
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

  - `Err` with an [`HttpError`](Web#HttpError) if it didn't succeed
  - `Ok` with the `Bytes`

-}
httpExpectBytes : HttpExpect (Result HttpError Bytes)
httpExpectBytes =
    HttpExpectBytes identity



-- Expect


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


{-| Create a `GET` [`HttpRequest`](Web#HttpRequest).

Use [`Web.httpAddHeaders`](Web#httpAddHeaders) to set custom headers as needed.
Use [`Web.timeOnceAt`](Web#timeOnceAt) to add a timeout of how long you are willing to wait before giving up.

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


{-| Add custom headers to the [`Web.HttpRequest`](Web#HttpRequest).

    request
        |> Web.httpAddHeaders
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



-- request


{-| Create a `POST` [`HttpRequest`](Web#HttpRequest).

Use [`Web.httpAddHeaders`](Web#httpAddHeaders) to set custom headers as needed.
Use [`Web.timeOnceAt`](Web#timeOnceAt) to add a timeout of how long you are willing to wait before giving up.

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


{-| An [`Interface`](Web#Interface) for handling an [`HttpRequest`](Web#HttpRequest)
using the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
-}
httpRequest : HttpRequest future -> Interface future
httpRequest request =
    request |> HttpRequest |> interfaceFromSingle


{-| An [`Interface`](Web#Interface) for downloading a given file
with [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/) as its content,
a given type and and a given default name.

Replacement for [`File.Download.bytes`](https://dark.elm.dmy.fr/packages/elm/file/latest/File-Download#bytes)

-}
fileDownloadBytes : { name : String, mimeType : String, content : Bytes } -> Interface future_
fileDownloadBytes fileDownloadConfig =
    FileDownloadUnsignedInt8s
        { name = fileDownloadConfig.name
        , mimeType = fileDownloadConfig.mimeType
        , content = fileDownloadConfig.content |> Bytes.LocalExtra.toUnsignedInt8List
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
windowResizeListen : Interface { width : Int, height : Int }
windowResizeListen =
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

Replacement for [`Browser.Navigation.replaceUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#replaceUrl)

-}
replaceUrl : AppUrl -> Interface future_
replaceUrl appUrl =
    NavigationReplaceUrl appUrl
        |> interfaceFromSingle



-- elm/browser on "How do I manage URL from a Browser.element?" https://github.com/elm/browser/blob/master/notes/navigation-in-elements.md


{-| An [`Interface`](Web#Interface) for changing the [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/)
and adding a new entry to the browser history
without triggering a page load.

Replacement for [`Browser.Navigation.pushUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#pushUrl)

-}
pushUrl : AppUrl -> Interface future_
pushUrl appUrl =
    NavigationPushUrl appUrl
        |> interfaceFromSingle


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


{-| If you used [`pushUrl`](#pushUrl) to update the URL with new history entries,
when the user clicks ← or → buttons (or you call [`navigateForward`](#navigateForward) or [`navigateBack`](#navigateBack) yourself),
the URL will change but your UI will not.

[`navigationListen`](#navigationListen) is an [`Interface`](Web#Interface) for detecting those URL changes and making ui changes as needed.

When the app itself initiates a url change with [`pushUrl`](#pushUrl) or [`replaceUrl`](#replaceUrl), no such event is triggered.

Note: This event is called ["popstate"](https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event) in js

-}
navigationListen : Interface AppUrl
navigationListen =
    WindowEventListen
        { eventName = "popstate"
        , on =
            Json.Decode.field "state"
                (Json.Decode.oneOf
                    [ Json.Decode.field "appUrl" AppUrl.LocalExtra.jsonDecoder
                    , Json.Decode.null () |> Json.Decode.map (\() -> AppUrl.fromPath [])
                    ]
                )
        }
        |> interfaceFromSingle


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


nodeFlattenToList :
    { path : List Int, node : DomNode future }
    -> List { path : List Int, node : DomNode future }
    -> List (InterfaceSingle future)
nodeFlattenToList current nodesRemaining =
    case current.node of
        DomText string ->
            ({ path = current.path |> List.reverse, node = DomHeaderText string }
                |> DomNodeRender
            )
                :: flattenRemainingNodesToList nodesRemaining

        DomElement element ->
            case element.subs of
                [] ->
                    ({ path = current.path |> List.reverse, node = DomElementHeader element.header }
                        |> DomNodeRender
                    )
                        :: flattenRemainingNodesToList nodesRemaining

                sub0 :: sub1Up ->
                    let
                        updatedRemaining : { index : Int, mapped : List { path : List Int, node : DomNode future } }
                        updatedRemaining =
                            sub1Up
                                |> List.foldr
                                    (\sub soFar ->
                                        { index = soFar.index - 1
                                        , mapped =
                                            { path = soFar.index :: current.path
                                            , node = sub
                                            }
                                                :: soFar.mapped
                                        }
                                    )
                                    { index = sub1Up |> List.length, mapped = nodesRemaining }
                    in
                    ({ path = current.path |> List.reverse, node = DomElementHeader element.header }
                        |> DomNodeRender
                    )
                        :: nodeFlattenToList
                            { path = 0 :: current.path, node = sub0 }
                            updatedRemaining.mapped


flattenRemainingNodesToList :
    List { path : List Int, node : DomNode future }
    -> List (InterfaceSingle future)
flattenRemainingNodesToList nodesRemaining =
    case nodesRemaining of
        [] ->
            []

        next :: remainingWithoutNext ->
            nodeFlattenToList next remainingWithoutNext


{-| An [`Interface`](Web#Interface) for displaying a given [`Web.DomNode`](Web#DomNode)
-}
domRender : DomNode future -> Interface future
domRender domNode =
    nodeFlattenToList { path = [], node = domNode } []
        |> internalFastDictFromSortedListMap
            (\interfaceSingle ->
                ( interfaceSingle |> interfaceSingleToStructuredId |> StructuredId.toString
                , interfaceSingle
                )
            )


{-| Builds a Dict from an already sorted list.

⚠️ DANGER ⚠️ This does _not_ check that the list is sorted.

-}
internalFastDictFromSortedListMap : (entry -> ( comparable, v )) -> List entry -> InternalFastDict comparable v
internalFastDictFromSortedListMap entryToKeyValue entries =
    let
        elementCount : Int
        elementCount =
            entries |> List.length

        redLayer : Int
        redLayer =
            toFloat elementCount |> logBase 2 |> floor

        go : Int -> Int -> Int -> List entry -> ( InternalFastDictInner comparable v, List entry )
        go layer fromIncluded toExcluded acc =
            if fromIncluded >= toExcluded then
                ( InternalFastDictLeaf, acc )

            else
                let
                    mid : Int
                    mid =
                        fromIncluded + (toExcluded - fromIncluded) // 2

                    ( lchild, accAfterLeft ) =
                        go (layer + 1) fromIncluded mid acc
                in
                case accAfterLeft of
                    [] ->
                        ( InternalFastDictLeaf, acc )

                    head :: tail ->
                        let
                            ( rchild, accAfterRight ) =
                                go (layer + 1) (mid + 1) toExcluded tail

                            color : InternalFastDictNColor
                            color =
                                if layer > 0 && layer == redLayer then
                                    InternalFastDictRed

                                else
                                    InternalFastDictBlack

                            ( k, v ) =
                                head |> entryToKeyValue
                        in
                        ( InternalFastDictInnerNode color k v lchild rchild
                        , accAfterRight
                        )
    in
    InternalFastDict elementCount
        (go 0 0 elementCount entries
            |> Tuple.first
        )


{-| Wire events from this [`Web.DomNode`](Web#DomNode) to a specific event, for example

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
domFutureMap : (future -> mappedFuture) -> (DomNode future -> DomNode mappedFuture)
domFutureMap futureChange domElementToMap =
    case domElementToMap of
        DomText string ->
            DomText string

        DomElement element ->
            element |> elementFutureMap futureChange |> DomElement


elementFutureMap : (future -> mappedFuture) -> (DomElement future -> DomElement mappedFuture)
elementFutureMap futureChange domElementToMap =
    { header = domElementToMap.header |> domElementHeaderFutureMap futureChange
    , subs =
        domElementToMap.subs |> List.map (\node -> node |> domFutureMap futureChange)
    }


{-| Plain text [`DomNode`](#DomNode)
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


{-| Sort a given list of { key, value } elements
by a given comparable representation of the key
to create a [`SortedKeyValueList`](Web#SortedKeyValueList)
-}
sortedKeyValueListFromListBy :
    (key -> comparable_)
    -> (List { value : value, key : key } -> SortedKeyValueList key value)
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

To create SVG elements, use [`Web.svgElement`](Web#svgElement)

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
use [`domListenToPreventingDefaultAction`](#domListenToPreventingDefaultAction)

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
    -> (DomModifier future -> DomModifier mappedFuture)
domModifierFutureMap futureChange modifier =
    modifier
        |> Rope.LocalExtra.mapFast
            (\modifierSingle ->
                modifierSingle |> domModifierSingleMap futureChange
            )


domModifierSingleMap :
    (future -> mappedFuture)
    -> (DomModifierSingle future -> DomModifierSingle mappedFuture)
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
audioStereoPan : AudioParameterTimeline -> (Audio -> Audio)
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
audioSpeedScaleBy : AudioParameterTimeline -> (Audio -> Audio)
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
audioVolumeScaleBy : AudioParameterTimeline -> (Audio -> Audio)
audioVolumeScaleBy volumeScaleFactor audio =
    { audio
        | volume =
            audio.volume
                |> audioParameterScaleAlongParameter audio.startTime
                    volumeScaleFactor
    }


audioAddProcessing : AudioProcessing -> (Audio -> Audio)
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
audioAddLinearConvolutionWith : AudioSource -> (Audio -> Audio)
audioAddLinearConvolutionWith bufferAudioSource audio =
    audio |> audioAddProcessing (AudioLinearConvolution { sourceUrl = bufferAudioSource.url })


{-| Frequencies below a given cutoff [parameter](Web#AudioParameterTimeline) pass through;
frequencies above it are attenuated.

Has a 12dB/octave rolloff and no peak at the cutoff.

-}
audioAddLowpassUntilFrequency : AudioParameterTimeline -> (Audio -> Audio)
audioAddLowpassUntilFrequency cutoffFrequency audio =
    audio |> audioAddProcessing (AudioLowpass { cutoffFrequency = cutoffFrequency })


{-| Frequencies below a given cutoff [parameter](Web#AudioParameterTimeline) are attenuated;
frequencies above it pass through.

Has a 12dB/octave rolloff and no peak at the cutoff.

-}
audioAddHighpassFromFrequency : AudioParameterTimeline -> (Audio -> Audio)
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


audioParameterValuesAlter : (Float -> Float) -> (AudioParameterTimeline -> AudioParameterTimeline)
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
audioParameterThrough : Time.Posix -> Float -> (AudioParameterTimeline -> AudioParameterTimeline)
audioParameterThrough keyFrameMoment keyFrameValue audioParameterTimelineSoFar =
    { startValue = audioParameterTimelineSoFar.startValue
    , keyFrames =
        { time = keyFrameMoment, value = keyFrameValue } :: audioParameterTimelineSoFar.keyFrames
    }


audioParameterScaleAlongParameter : Time.Posix -> AudioParameterTimeline -> (AudioParameterTimeline -> AudioParameterTimeline)
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
    -> (List { time : Time.Posix, value : Float } -> List { time : Time.Posix, value : Float })
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
    -> (List { time : Time.Posix, value : Float } -> List { time : Time.Posix, value : Float })
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
