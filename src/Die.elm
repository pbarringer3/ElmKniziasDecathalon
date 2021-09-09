module Die exposing
    ( Die
    , asInt
    , blank
    , rolledDie
    , toSvg
    )

import Array
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


toSvg : Int -> Die -> Html.Html msg
toSvg size die =
    -- Will not allow dice size to be less than 20 pixels (24 with margin)
    let
        boundedSize =
            max size 20

        margin =
            boundedSize // 10

        boxSize =
            String.fromInt (boundedSize + 2 * margin)

        strokeWidth =
            String.fromInt (max (boundedSize // 30) 1)
    in
    Svg.svg
        [ SA.width boxSize
        , SA.height boxSize
        , SA.viewBox ("0 0 " ++ boxSize ++ " " ++ boxSize)
        ]
        (List.append
            [ Svg.rect
                [ SA.x (String.fromInt margin)
                , SA.y (String.fromInt margin)
                , SA.width (String.fromInt boundedSize)
                , SA.height (String.fromInt boundedSize)
                , SA.fill "white"
                , SA.stroke "black"
                , SA.strokeWidth strokeWidth
                ]
                []
            ]
            (pips (asInt die) boundedSize)
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

        margin =
            size // 10

        offset =
            quarter + margin
    in
    ( Tuple.first location * quarter + offset, Tuple.second location * quarter + offset )


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
