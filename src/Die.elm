module Die exposing
    ( Die
    , asInt
    , blank
    , rolledDie
    , toSvgElement
    )

import Array
import Element exposing (Element)
import Html
import Random
import Svg
import Svg.Attributes as SA



-- Represents a D6. The Int is the value of the face.
-- A value of 0 represents an unrolled (blank) Die.


type Die
    = Die Int


asInt : Die -> Int
asInt die =
    case die of
        Die value ->
            value


blank : Die
blank =
    Die 0


rolledDie : Random.Generator Die
rolledDie =
    Random.map (\faceVal -> Die faceVal) (Random.int 1 6)


toSvgElement : Int -> Die -> Element msg
toSvgElement size die =
    -- Will not allow dice size to be less than 20 pixels (24 with margin)
    let
        boundedSize =
            max size 20

        boxSize =
            String.fromInt boundedSize

        strokeWidth =
            String.fromInt (max (boundedSize // 25) 1)
    in
    Element.el []
        (Svg.svg
            [ SA.width boxSize
            , SA.height boxSize
            , SA.viewBox ("0 0 " ++ boxSize ++ " " ++ boxSize)
            ]
            (List.append
                [ Svg.rect
                    [ SA.x "0"
                    , SA.y "0"
                    , SA.width boxSize
                    , SA.height boxSize
                    , SA.fill "white"
                    , SA.stroke "black"
                    , SA.strokeWidth strokeWidth
                    ]
                    []
                ]
                (pips (asInt die) boundedSize)
            )
            |> Element.html
        )


pipLocations : List (List ( Int, Int ))
pipLocations =
    [ [ ( 1, 1 ) ]
    , [ ( 0, 0 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 2, 0 ), ( 0, 2 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 2, 0 ), ( 1, 1 ), ( 0, 2 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 2, 0 ), ( 0, 1 ), ( 2, 1 ), ( 0, 2 ), ( 2, 2 ) ]
    ]


dieCoordinates : Int -> ( Int, Int ) -> ( Int, Int )
dieCoordinates size location =
    let
        quarter =
            size // 4
    in
    ( quarter * (Tuple.first location + 1), quarter * (Tuple.second location + 1) )


pip : Int -> ( Int, Int ) -> Svg.Svg msg
pip r coord =
    Svg.circle
        [ SA.cx (String.fromInt (Tuple.first coord))
        , SA.cy (String.fromInt (Tuple.second coord))
        , SA.r (String.fromInt r)
        ]
        []


pips : Int -> Int -> List (Svg.Svg msg)
pips value size =
    let
        pipRadius =
            max (size // 20) 1
    in
    pipLocations
        |> Array.fromList
        |> Array.get (value - 1)
        |> Maybe.withDefault []
        |> List.map (dieCoordinates size)
        |> List.map (pip pipRadius)
