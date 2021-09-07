-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--


module Main exposing (Model, Msg(..), die, dieCircles, dieCoordinates, dieLocations, init, main, pips, subscriptions, update, view)

import Array
import Browser
import Html exposing (button, div)
import Html.Events exposing (..)
import List
import Maybe
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFaces : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (List.repeat 4 0)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFaces (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFaces (Random.list (List.length model.dieFaces) (Random.int 1 6))
            )

        NewFaces newFaces ->
            ( Model newFaces
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        (List.append
            (List.map die model.dieFaces)
            [ button [ onClick Roll ] [ Html.text "Roll" ] ]
        )


die : Int -> Html.Html Msg
die face =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (List.append
            [ rect
                [ x "10"
                , y "10"
                , width "100"
                , height "100"
                , fill "white"
                , stroke "black"
                , strokeWidth "3"
                ]
                []
            ]
            (pips face)
        )


dieLocations : List (List ( Int, Int ))
dieLocations =
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


dieCircles : ( Int, Int ) -> Svg Msg
dieCircles coord =
    circle
        [ cx (String.fromInt (Tuple.first coord))
        , cy (String.fromInt (Tuple.second coord))
        , r "5"
        ]
        []


pips : Int -> List (Svg Msg)
pips face =
    dieLocations
        |> Array.fromList
        |> Array.get (face - 1)
        |> Maybe.withDefault []
        |> List.map dieCoordinates
        |> List.map dieCircles
