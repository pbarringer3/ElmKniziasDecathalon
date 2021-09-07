-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--


module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Die
import Html exposing (button, div)
import Html.Events exposing (..)
import List
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



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
    { dice : List Die.Die
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (List.repeat 4 Die.blank)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewDice (List Die.Die)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewDice (Random.list (List.length model.dice) Die.rolledDie)
            )

        NewDice newDice ->
            ( Model newDice
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
            (List.map Die.toSvg model.dice)
            [ button [ onClick Roll ] [ Html.text "Roll" ] ]
        )
