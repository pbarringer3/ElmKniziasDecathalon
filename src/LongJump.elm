module LongJump exposing (main)

import Array
import Browser
import Die
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Html
import List
import Random
import Set
import String exposing (fromInt)



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
    { attempt : Int
    , phase : Phase
    , freezableDice : List FreezableDie
    , runupDice : Set.Set Int
    , jumpDice : Set.Set Int
    }


type Phase
    = Runup
    | Jump


type alias FreezableDie =
    { index : Int
    , die : Die.Die
    , frozen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { attempt = 1
      , phase = Runup
      , freezableDice = getInitialDice 5
      , runupDice = Set.fromList <| List.range 0 4
      , jumpDice = Set.empty
      }
    , Cmd.none
    )


getInitialDice : Int -> List FreezableDie
getInitialDice num =
    Array.repeat num Die.blank
        |> Array.toIndexedList
        |> List.map (\tuple -> FreezableDie (Tuple.first tuple) (Tuple.second tuple) False)



-- UPDATE


type Msg
    = Roll Phase
    | Freeze Int
    | BeginJump


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll _ ->
            ( model, Cmd.none )

        Freeze _ ->
            ( model, Cmd.none )

        BeginJump ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    layout []
        (column [ width fill ]
            [ text <| fromInt model.attempt ]
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
