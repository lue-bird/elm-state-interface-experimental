module Web.Audio exposing
    ( sourceLoad, fromSource, play
    , volumeScaleBy, speedScaleBy, stereoPan
    , addLinearConvolutionWith, addHighpassFromFrequency, addLowpassUntilFrequency
    )

{-| Play [`Audio`](Web#Audio) as part of an [`Interface`](Web#Interface).

    import Time
    import Web
    import Web.Audio

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
                            Web.Audio.fromSource audioSource startTime
                                |> Web.Audio.play


                        Nothing ->
                            Web.Time.posixRequest
                                |> Web.interfaceFutureMap
                                    (\time -> { state | audioSTartTime = time |> Just })

                Nothing ->
                    Web.Audio.sourceLoad "https://s3-us-west-2.amazonaws.com/s.cdpn.io/123941/Yodel_Sound_Effect.mp3"
                        |> Web.interfaceFutureMap
                            (\result -> { state | audioSource = result |> Just })

                Just (Err _) ->
                    Web.Console.error "audio failed to load"
    }

@docs sourceLoad, fromSource, play
@docs volumeScaleBy, speedScaleBy, stereoPan
@docs addLinearConvolutionWith, addHighpassFromFrequency, addLowpassUntilFrequency

To detune, use [`speedScaleBy`](#speedScaleBy). It's documentation also shows which scale relates to which semitone pitch.

-}

import Duration
import Rope
import Time
import Web
import Web.Audio.Parameter
import Web.Audio.Parameter.Internal


{-| Change the stereo panning with a given a signed percentage [parameter](Web#AudioParameterTimeline).

`Web.Audio.pan -0.9` for example means that the sound is almost fully balanced towards the left speaker

-}
stereoPan : Web.AudioParameterTimeline -> (Web.Audio -> Web.Audio)
stereoPan signedPercentageTimeline audio =
    { audio
        | stereoPan =
            audio.stereoPan
                |> Web.Audio.Parameter.Internal.scaleAlongParameter audio.startTime
                    signedPercentageTimeline
    }


{-| Scale the playback rate by a given factor. This will also affect pitch.

For example, `Web.Audio.speedScaleBy 0.5` means playback will take twice as long and the pitch will be one octave lower, see [AudioBufferSourceNode.playbackRate](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/playbackRate)

In general, to pitch by semitones:

    Web.Audio.speedScaleBy
        (Web.Audio.Parameter.at (2 ^ (semitones / 12)))

Note: It would be possible to modify the signal to compensate for the pitch change,
see [Audio time stretching and pitch scaling](https://en.wikipedia.org/wiki/Audio_time_stretching_and_pitch_scaling).
Help appreciated!

-}
speedScaleBy : Web.AudioParameterTimeline -> (Web.Audio -> Web.Audio)
speedScaleBy speedScaleFactorTimeline audio =
    { audio
        | speed =
            audio.speed
                |> Web.Audio.Parameter.Internal.scaleAlongParameter audio.startTime
                    speedScaleFactorTimeline
    }


{-| Scale how loud it is.
1 preserves the current volume, 0.5 halves it, and 0 mutes it.
If the the volume is less than 0, 0 will be used instead.
-}
volumeScaleBy : Web.AudioParameterTimeline -> (Web.Audio -> Web.Audio)
volumeScaleBy volumeScaleFactor audio =
    { audio
        | volume =
            audio.volume
                |> Web.Audio.Parameter.Internal.scaleAlongParameter audio.startTime
                    volumeScaleFactor
    }


addProcessing : Web.AudioProcessing -> (Web.Audio -> Web.Audio)
addProcessing newLastProcessing audio =
    { audio
        | processingLastToFirst = audio.processingLastToFirst |> (::) newLastProcessing
    }


{-| Usually used to apply reverb and or echo.
Given a loaded [`AudioSource`](Web#AudioSource) containing the impulse response,
it performs a [Convolution](https://en.wikipedia.org/wiki/Convolution) with the [`Audio`](Web#Audio)

If you need some nice impulse wavs to try it out, there's a few at [`dhiogoboza/audio-convolution`](https://github.com/dhiogoboza/audio-convolution/tree/master/impulses).
If you know more nice ones, don't hesitate to open an issue or a PR.

-}
addLinearConvolutionWith : Web.AudioSource -> (Web.Audio -> Web.Audio)
addLinearConvolutionWith bufferAudioSource audio =
    audio |> addProcessing (Web.AudioLinearConvolution { sourceUrl = bufferAudioSource.url })


{-| Frequencies below a given cutoff [parameter](Web#AudioParameterTimeline) pass through;
frequencies above it are attenuated.

Has a 12dB/octave rolloff and no peak at the cutoff.

-}
addLowpassUntilFrequency : Web.AudioParameterTimeline -> (Web.Audio -> Web.Audio)
addLowpassUntilFrequency cutoffFrequency audio =
    audio |> addProcessing (Web.AudioLowpass { cutoffFrequency = cutoffFrequency })


{-| Frequencies below a given cutoff [parameter](Web#AudioParameterTimeline) are attenuated;
frequencies above it pass through.

Has a 12dB/octave rolloff and no peak at the cutoff.

-}
addHighpassFromFrequency : Web.AudioParameterTimeline -> (Web.Audio -> Web.Audio)
addHighpassFromFrequency cutoffFrequency audio =
    audio |> addProcessing (Web.AudioHighpass { cutoffFrequency = cutoffFrequency })


{-| Create [`Audio`](Web#Audio) from an given loaded [source](Web#AudioSource)
which will play at a given [time](https://dark.elm.dmy.fr/packages/elm/time/latest/)

    -- play a song at half speed and wait 2 seconds after the usual song start time before starting
    Web.Audio.fromSource
        myCoolSong
        (Duration.addTo usualSongStartTime (Duration.seconds 2))
        |> Web.Audio.speedScaleBy (Web.Audio.Parameter.at 0.5)

Note that in some browsers audio will be muted until the user interacts with the webpage.

-}
fromSource : Web.AudioSource -> Time.Posix -> Web.Audio
fromSource source startTime =
    { url = source.url
    , startTime = startTime
    , volume = Web.Audio.Parameter.at 1
    , speed = Web.Audio.Parameter.at 1
    , stereoPan = Web.Audio.Parameter.at 0
    , processingLastToFirst = []
    }


{-| An [`Interface`](Web#Interface) for fetching audio data from a given url
and returning an [`AudioSource`](Web#AudioSource) to use with [`fromSource`](#fromSource).
-}
sourceLoad : String -> Web.Interface (Result Web.AudioSourceLoadError Web.AudioSource)
sourceLoad url =
    Web.AudioSourceLoad { url = url, on = identity }
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for playing [`Audio`](Web#Audio) created with [`Audio.fromSource`](#fromSource).

To play multiple audios:

    [ audio0, audio1, audio2 ]
        |> List.map Web.Audio.play
        |> Web.interfaceBatch

-}
play : Web.Audio -> Web.Interface future_
play audio =
    Web.AudioPlay
        { audio
            | startTime = Duration.addTo audio.startTime (Duration.milliseconds 50)
            , volume = audio.volume |> Web.Audio.Parameter.Internal.valuesAlter (\value -> Basics.max 0 value)

            -- negative speed values are supported by some browsers
            -- https://stackoverflow.com/questions/9874167/how-can-i-play-audio-in-reverse-with-web-audio-api/9875011#9875011
        }
        |> Rope.singleton
