module Web.Dom exposing
    ( Node(..), text
    , Element, element, elementNamespaced
    , futureMap, render
    , Modifier, modifierFutureMap, modifierBatch, modifierNone
    , attribute, attributeNamespaced, style, boolProperty, stringProperty
    , listenTo, listenToPreventingDefaultAction
    , scrollToShow, scrollPositionRequest, scrollToPosition
    , ModifierSingle(..)
    )

{-| Helpers for [DOM nodes](#Node) as part of an [`Interface`](Web#Interface).

These are primitives used for [svg](Web-Svg) and html
(filling the same role as [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/))

@docs Node, text
@docs Element, element, elementNamespaced
@docs futureMap, render
@docs Modifier, modifierFutureMap, modifierBatch, modifierNone
@docs attribute, attributeNamespaced, style, boolProperty, stringProperty
@docs listenTo, listenToPreventingDefaultAction
@docs scrollToShow, scrollPositionRequest, scrollToPosition


## internals, safe to ignore for users

Exposed so can for example simulate it more easily in tests, add a debugger etc.

@docs ModifierSingle

-}

import Json.Decode
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Web


nodeFlattenToList :
    List (Web.InterfaceSingle future)
    -> { path : List Int, node : Node future }
    -> List { path : List Int, node : Node future }
    -> List (Web.InterfaceSingle future)
nodeFlattenToList interfacesSoFar current nodesRemaining =
    case current.node of
        Text string ->
            flattenRemainingNodesToList
                (({ pathReverse = current.path, node = Web.DomText string }
                    |> Web.DomNodeRender
                 )
                    :: interfacesSoFar
                )
                nodesRemaining

        Element element_ ->
            let
                updatedInterfaces : List (Web.InterfaceSingle future)
                updatedInterfaces =
                    ({ pathReverse = current.path, node = Web.DomElementHeader element_.header }
                        |> Web.DomNodeRender
                    )
                        :: interfacesSoFar
            in
            case element_.subs of
                [] ->
                    flattenRemainingNodesToList updatedInterfaces nodesRemaining

                sub0 :: sub1Up ->
                    let
                        updatedRemaining : { index : Int, mapped : List { path : List Int, node : Node future } }
                        updatedRemaining =
                            sub1Up
                                |> List.foldl
                                    (\sub soFar ->
                                        { index = soFar.index + 1
                                        , mapped =
                                            { path = soFar.index :: current.path
                                            , node = sub
                                            }
                                                :: soFar.mapped
                                        }
                                    )
                                    { index = 1, mapped = nodesRemaining }
                    in
                    nodeFlattenToList
                        updatedInterfaces
                        { path = 0 :: current.path, node = sub0 }
                        updatedRemaining.mapped


{-| An [`Interface`](Web#Interface) for displaying a given [`Web.Dom.Node`](Web-Dom#Node)
-}
render : Node future -> Web.Interface future
render =
    \domNode ->
        nodeFlattenToList [] { path = [], node = domNode } []
            |> Rope.fromList


flattenRemainingNodesToList :
    List (Web.InterfaceSingle future)
    -> List { path : List Int, node : Node future }
    -> List (Web.InterfaceSingle future)
flattenRemainingNodesToList updatedInterfaces nodesRemaining =
    case nodesRemaining of
        [] ->
            updatedInterfaces

        next :: remainingWithoutNext ->
            nodeFlattenToList updatedInterfaces next remainingWithoutNext


{-| Wire events from this [`Web.Dom.Node`](Web-Dom#Node) to a specific event, for example

    buttonUi "start"
        |> Web.Dom.futureMap (\Clicked -> StartButtonClicked)

with

    buttonUi : String -> Web.Dom.Node ButtonEvent
    buttonUi label =
        Web.Dom.element "button"
            [ Web.Dom.listenTo "click"
                |> Web.Dom.modifierFutureMap (\_ -> Clicked)
            ]
            [ Web.Dom.text label ]

    type ButtonEvent
        = Clicked

-}
futureMap : (future -> mappedFuture) -> (Node future -> Node mappedFuture)
futureMap futureChange =
    \domElementToMap ->
        case domElementToMap of
            Text string ->
                Text string

            Element domElement ->
                domElement |> elementFutureMap futureChange |> Element


elementFutureMap : (future -> mappedFuture) -> (Element future -> Element mappedFuture)
elementFutureMap futureChange =
    \domElementToMap ->
        { header = domElementToMap.header |> domElementHeaderFutureMap futureChange
        , subs =
            domElementToMap.subs |> List.map (\node -> node |> futureMap futureChange)
        }


domElementHeaderFutureMap :
    (future -> mappedFuture)
    -> (Web.DomElementHeader future -> Web.DomElementHeader mappedFuture)
domElementHeaderFutureMap futureChange =
    \domElementToMap ->
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
                |> List.map
                    (\entry ->
                        { key = entry.key
                        , value =
                            { on = \event -> entry.value.on event |> futureChange
                            , defaultActionHandling = entry.value.defaultActionHandling
                            }
                        }
                    )
        }


{-| Plain text [DOM `Node`](#Node)
-}
text : String -> Node future_
text =
    Text


elementWithMaybeNamespace :
    Maybe String
    -> String
    -> List (Modifier future)
    -> List (Node future)
    -> Node future
elementWithMaybeNamespace maybeNamespace tag modifiers subs =
    let
        modifiersFlat :
            { namespace : Maybe String
            , tag : String
            , scrollToPosition : Maybe { fromLeft : Float, fromTop : Float }
            , scrollToShow : Maybe { x : Web.DomElementVisibilityAlignment, y : Web.DomElementVisibilityAlignment }
            , scrollPositionRequest : Maybe ({ fromLeft : Float, fromTop : Float } -> future)
            , eventListens : List { key : String, value : { on : Json.Decode.Value -> future, defaultActionHandling : Web.DefaultActionHandling } }
            , styles : List { key : String, value : String }
            , stringProperties : List { key : String, value : String }
            , boolProperties : List { key : String, value : Bool }
            , attributesNamespaced : List { key : ( String, String ), value : String }
            , attributes : List { key : String, value : String }
            }
        modifiersFlat =
            modifiers
                |> modifierBatch
                |> Rope.foldr
                    (\modifier soFar ->
                        case modifier of
                            ScrollToPosition position ->
                                { soFar | scrollToPosition = position |> Just }

                            ScrollToShow alignment ->
                                { soFar | scrollToShow = alignment |> Just }

                            ScrollPositionRequest positionRequest ->
                                { soFar | scrollPositionRequest = positionRequest |> Just }

                            Listen listen ->
                                { soFar
                                    | eventListens =
                                        soFar.eventListens
                                            |> (::)
                                                { key = listen.eventName
                                                , value =
                                                    { on = listen.on
                                                    , defaultActionHandling = listen.defaultActionHandling
                                                    }
                                                }
                                }

                            Style keyValue ->
                                { soFar | styles = soFar.styles |> (::) keyValue }

                            StringProperty keyValue ->
                                { soFar
                                    | stringProperties =
                                        soFar.stringProperties |> (::) keyValue
                                }

                            BoolProperty keyValue ->
                                { soFar
                                    | boolProperties =
                                        soFar.boolProperties |> (::) keyValue
                                }

                            Attribute keyValue ->
                                case keyValue.namespace of
                                    Just namespace ->
                                        { soFar
                                            | attributesNamespaced =
                                                soFar.attributesNamespaced
                                                    |> (::) { key = ( namespace, keyValue.key ), value = keyValue.value }
                                        }

                                    Nothing ->
                                        { soFar
                                            | attributes =
                                                soFar.attributes
                                                    |> (::) { key = keyValue.key, value = keyValue.value }
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
        , eventListens = modifiersFlat.eventListens
        , styles = modifiersFlat.styles
        , stringProperties = modifiersFlat.stringProperties
        , boolProperties = modifiersFlat.boolProperties
        , attributes = modifiersFlat.attributes
        , attributesNamespaced = modifiersFlat.attributesNamespaced
        }
    , subs = subs
    }
        |> Element


{-| Create a DOM element with a given tag, [`Modifier`](#Modifier)s and sub-[node](Web-Dom#Node)s.
For example to get `<p>flying</p>`

    Web.Dom.element "p"
        []
        [ Web.Dom.text "flying" ]

To create SVG elements, use [`Web.Svg.element`](Web-Svg#element)

-}
element : String -> List (Modifier future) -> List (Node future) -> Node future
element tag modifiers subs =
    elementWithMaybeNamespace Nothing tag modifiers subs


{-| Create a DOM element with a given namespace, tag, [`Modifier`](#Modifier)s and sub-[node](Web-Dom#Node)s.
For example, [`Web.Svg`](Web-Svg) defines its elements using

    element : String -> List (Modifier future) -> List (DomNode future) -> DomNode future
    element tag modifiers subs =
        Web.Dom.elementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs

-}
elementNamespaced :
    String
    -> String
    -> List (Modifier future)
    -> List (Node future)
    -> Node future
elementNamespaced namespace tag modifiers subs =
    elementWithMaybeNamespace (namespace |> Just) tag modifiers subs


{-| Setting of a [`Web.Dom.Element`](Web-Dom#Element).
To create one, use [`attribute`](#attribute), [`style`](#style), [`listenTo`](#listenTo) etc.
To combine multiple, use [`Web.Dom.modifierBatch`](#modifierBatch) and [`Web.Dom.modifierNone`](#modifierNone)

For example to get `<a href="https://elm-lang.org">elm</a>`

    Web.Dom.element "a"
        [ Web.Dom.attribute "href" "https://elm-lang.org" ]
        [ Web.Dom.text "elm" ]

Btw: If you can think of a nicer name for this like "customization", "characteristic" or "aspect",
please [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new).


## attribute vs property

  - attribute: part of the HTML itself. E.g. `class` in `<div class="greeting"></div>`
  - property: an actual field on that js DOM object. E.g. `className` in `div.className = "greeting"`

But don't be surprised: There are cases where

  - only a property _or_ an attribute of a name exists
  - both exist but they have different effects

For example, trying to reset the text inside a a text input with

    Web.Dom.attribute "value" "user input"

    -- then later replace it with
    Web.Dom.attribute "value" ""

will only provide a _default value_ and has no effect on the currently written text,
so you'll have to use

    Web.Dom.stringProperty "value" ""

Similarly for checkboxes:

    Web.Dom.boolProperty "checked" False

Maybe a rule of thumb is:
Use properties to set anything related to interactivity
and attributes for everything else.

If you have some opinions or better explanations,
please [open an issue](https://github.com/lue-bird/elm-state-interface-experimental/issues/new).

-}
type alias Modifier future =
    Rope (ModifierSingle future)


{-| Combine multiple [`Modifier`](#Modifier)s into one.
-}
modifierBatch : List (Modifier future) -> Modifier future
modifierBatch =
    \modifiers -> modifiers |> Rope.fromList |> Rope.concat


{-| Doing nothing as a [`Modifier`](#Modifier). These two examples are equivalent:

    Web.Dom.modifierBatch
        [ a, Web.Dom.modifierNone, b ]

and

    Web.Dom.modifierBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
modifierNone : Modifier future_
modifierNone =
    Rope.empty


{-| An individual [`Modifier`](#Modifier).
Create using [`attribute`](#attribute), [`style`](#style), [`listenTo`](#listenTo) etc.
-}
type ModifierSingle future
    = Attribute { namespace : Maybe String, key : String, value : String }
    | StringProperty { key : String, value : String }
    | BoolProperty { key : String, value : Bool }
    | Style { key : String, value : String }
    | ScrollToPosition { fromLeft : Float, fromTop : Float }
    | ScrollToShow { x : Web.DomElementVisibilityAlignment, y : Web.DomElementVisibilityAlignment }
    | ScrollPositionRequest ({ fromLeft : Float, fromTop : Float } -> future)
    | Listen
        { eventName : String
        , on : Json.Decode.Value -> future
        , defaultActionHandling : Web.DefaultActionHandling
        }


{-| A key-value attribute [`Modifier`](#Modifier)
-}
attribute : String -> String -> Modifier future_
attribute key value =
    { namespace = Nothing, key = key, value = value } |> Attribute |> Rope.singleton


{-| A key-string value DOM property [`Modifier`](#Modifier)
-}
stringProperty : String -> String -> Modifier future_
stringProperty key value =
    { key = key, value = value } |> StringProperty |> Rope.singleton


{-| A key-bool value DOM property [`Modifier`](#Modifier)
-}
boolProperty : String -> Bool -> Modifier future_
boolProperty key value =
    { key = key, value = value } |> BoolProperty |> Rope.singleton


{-| A namespaced key-value attribute [`Modifier`](#Modifier).
For example, you could define an SVG xlink href attribute as

    attributeXlinkHref : String -> Modifier msg
    attributeXlinkHref value =
        Web.Dom.attributeNamespaced "http://www.w3.org/1999/xlink" "xlink:href" value

-}
attributeNamespaced : String -> String -> String -> Modifier future_
attributeNamespaced namespace key value =
    { namespace = namespace |> Just, key = key, value = value } |> Attribute |> Rope.singleton


{-| A key-value style [`Modifier`](#Modifier)
-}
style : String -> String -> Modifier future_
style key value =
    { key = key, value = value } |> Style |> Rope.singleton


{-| Listen for a specific DOM event on the [`Web.Dom.Element`](Web-Dom#Element).
Use [`modifierFutureMap`](#modifierFutureMap) to wire this to a specific event.

If you want to override the browser's default behavior for that event,
use [`listenToPreventingDefaultAction`](#listenToPreventingDefaultAction)

-}
listenTo : String -> Modifier Json.Decode.Value
listenTo eventName =
    { eventName = eventName
    , on = identity
    , defaultActionHandling = Web.DefaultActionExecute
    }
        |> Listen
        |> Rope.singleton


{-| Like [`listenTo`](#listenTo) but [preventing the browser's default action](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault).

That's for example how elm's [`Browser.Events.onSubmit`](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#onSubmit)
prevents the form from changing the page’s location:

    submitListen : Web.Dom.Modifier ()
    submitListen =
        Web.Dom.listenToPreventingDefaultAction "submit"
            |> Web.Dom.modifierFutureMap (\_ -> ())

-}
listenToPreventingDefaultAction : String -> Modifier Json.Decode.Value
listenToPreventingDefaultAction eventName =
    { eventName = eventName
    , on = identity
    , defaultActionHandling = Web.DefaultActionPrevent
    }
        |> Listen
        |> Rope.singleton


{-| Wire events from this [`Modifier`](#Modifier) to a specific event.

    Web.Dom.listen "click" |> Web.Dom.modifierFutureMap (\_ -> ButtonClicked)

-}
modifierFutureMap :
    (future -> mappedFuture)
    -> (Modifier future -> Modifier mappedFuture)
modifierFutureMap futureChange =
    \modifier ->
        modifier |> Rope.map (\modifierSingle -> modifierSingle |> modifierSingleMap futureChange)


modifierSingleMap :
    (future -> mappedFuture)
    -> (ModifierSingle future -> ModifierSingle mappedFuture)
modifierSingleMap futureChange =
    \modifier ->
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

Use in combination with [`scrollToPosition`](#scrollToPosition)
to implement saving and restoring scroll position even when users had navigated off a URL.

-}
scrollPositionRequest : Modifier { fromLeft : Float, fromTop : Float }
scrollPositionRequest =
    ScrollPositionRequest identity |> Rope.singleton


{-| Ensure a given initial scroll position in both directions.
To move to the edge in a direction, use [`scrollToShow`](#scrollToShow) instead.

Unlike [`style`](#style)s,
this is just an initial configuration
which can be changed by user actions.
So adding e.g. `scrollToPosition ...`
will scroll once the next render happens
but will not prevent users from scrolling away.

-}
scrollToPosition :
    { fromLeft : Float, fromTop : Float }
    -> Modifier future_
scrollToPosition position =
    ScrollToPosition position |> Rope.singleton


{-| Ensure a given initial [`DomElementVisibilityAlignment`](Web#DomElementVisibilityAlignment)
in both directions.

Unlike [`style`](#style)s,
this is just an initial configuration
which can be changed by user actions.
So adding e.g. `scrollToShow { y = Web.DomElementStart, x = Web.DomElementStart }`
will scroll to the top left once the next render happens
but will not prevent users from scrolling away.

Note: Uses [`Element.scrollIntoView`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView)

-}
scrollToShow :
    { x : Web.DomElementVisibilityAlignment, y : Web.DomElementVisibilityAlignment }
    -> Modifier future_
scrollToShow preferredAlignment =
    ScrollToShow preferredAlignment |> Rope.singleton


{-| Plain text or an [`Element`](#Element). Create using [`text`](#text) and [`element`](#element)
-}
type Node future
    = Text String
    | Element (Element future)


{-| A tagged DOM node that can itself contain child [node](#Node)s
-}
type alias Element future =
    RecordWithoutConstructorFunction
        { header : Web.DomElementHeader future
        , subs : List (Node future)
        }
