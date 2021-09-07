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


toSvg : Die -> Html.Html msg
toSvg die =
    Svg.svg
        [ SA.width "120"
        , SA.height "120"
        , SA.viewBox "0 0 120 120"
        ]
        (List.append
            [ Svg.rect
                [ SA.x "10"
                , SA.y "10"
                , SA.width "100"
                , SA.height "100"
                , SA.fill "white"
                , SA.stroke "black"
                , SA.strokeWidth "3"
                ]
                []
            ]
            (pips (asInt die))
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


dieCoordinates : ( Int, Int ) -> ( Int, Int )
dieCoordinates location =
    ( Tuple.first location * 25 + 35, Tuple.second location * 25 + 35 )


pip : ( Int, Int ) -> Svg.Svg msg
pip coord =
    Svg.circle
        [ SA.cx (String.fromInt (Tuple.first coord))
        , SA.cy (String.fromInt (Tuple.second coord))
        , SA.r "5"
        ]
        []


pips : Int -> List (Svg.Svg msg)
pips value =
    pipLocations
        |> Array.fromList
        |> Array.get (value - 1)
        |> Maybe.withDefault []
        |> List.map dieCoordinates
        |> List.map pip
