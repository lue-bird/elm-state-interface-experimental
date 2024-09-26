module Svg.LocalExtra exposing (circle, ellipse, fillUniform, line, polygon, polyline, rotated, strokeUniform, strokeWidth)

import Angle
import Color exposing (Color)
import Web


line :
    { start : { x : Float, y : Float }, end : { x : Float, y : Float } }
    -> List (Web.DomModifier future)
    -> Web.DomNode future
line lineGeometry additionalModifiers =
    Web.svgElement "line"
        ([ Web.domAttribute "x1" (lineGeometry.start.x |> String.fromFloat)
         , Web.domAttribute "y1" (lineGeometry.start.y |> String.fromFloat)
         , Web.domAttribute "x2" (lineGeometry.end.x |> String.fromFloat)
         , Web.domAttribute "y2" (lineGeometry.end.y |> String.fromFloat)
         ]
            ++ additionalModifiers
        )
        []


circle : { position : { x : Float, y : Float }, radius : Float } -> List (Web.DomModifier future) -> Web.DomNode future
circle geometry additionalModifiers =
    Web.svgElement "circle"
        ([ Web.domAttribute "cx" ((geometry.position.x |> String.fromFloat) ++ "px")
         , Web.domAttribute "cy" ((geometry.position.y |> String.fromFloat) ++ "px")
         , Web.domAttribute "r" ((geometry.radius |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


ellipse : { position : { x : Float, y : Float }, radiusX : Float, radiusY : Float } -> List (Web.DomModifier future) -> Web.DomNode future
ellipse geometry additionalModifiers =
    Web.svgElement "ellipse"
        ([ Web.domAttribute "cx" ((geometry.position.x |> String.fromFloat) ++ "px")
         , Web.domAttribute "cy" ((geometry.position.y |> String.fromFloat) ++ "px")
         , Web.domAttribute "rx" ((geometry.radiusX |> String.fromFloat) ++ "px")
         , Web.domAttribute "ry" ((geometry.radiusY |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


rotated : { angle : Angle.Angle, center : { x : Float, y : Float } } -> Web.DomModifier future_
rotated geometry =
    Web.domAttribute "transform"
        ([ "rotate("
         , geometry.angle |> Angle.inDegrees |> String.fromFloat
         , ", "
         , geometry.center.x |> String.fromFloat
         , ", "
         , geometry.center.y |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


fillUniform : Color -> Web.DomModifier future_
fillUniform color =
    Web.domAttribute "fill" (color |> Color.toCssString)


strokeUniform : Color -> Web.DomModifier future_
strokeUniform color =
    Web.domAttribute "stroke" (color |> Color.toCssString)


points : List { x : Float, y : Float } -> Web.DomModifier future_
points =
    \points_ ->
        Web.domAttribute "points"
            ((case points_ of
                [ onlyElement ] ->
                    [ onlyElement, onlyElement ]

                notOnlyOne ->
                    notOnlyOne
             )
                |> List.map (\point -> [ point.x |> String.fromFloat, ",", point.y |> String.fromFloat ] |> String.concat)
                |> String.join " "
            )


polygon : List { x : Float, y : Float } -> List (Web.DomModifier future) -> Web.DomNode future
polygon points_ additionalModifiers =
    Web.svgElement "polyline"
        (points points_
            :: additionalModifiers
        )
        []


strokeWidth : Float -> Web.DomModifier future_
strokeWidth pixels =
    Web.domAttribute "stroke-width" ((pixels |> String.fromFloat) ++ "px")


polyline : List { x : Float, y : Float } -> List (Web.DomModifier future) -> Web.DomNode future
polyline points_ additionalModifiers =
    Web.svgElement "polyline"
        (points points_
            :: additionalModifiers
        )
        []
