-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--


module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Css exposing (..)
import Die
import Html exposing (br, button, div)
import Html.Events exposing (..)
import Html.Styled.Attributes exposing (css)
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
    { rerolls : Int
    , firstGroup : List Die.Die
    , secondGroup : List Die.Die
    , phase : Phase
    , score : Int
    }


type Phase
    = One
    | Two


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rerolls = 5
      , firstGroup = List.repeat 4 Die.blank
      , secondGroup = List.repeat 4 Die.blank
      , phase = One
      , score = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Reroll
    | NewDice (List Die.Die)
    | FreezeOne
    | FreezeTwo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewDice (Random.list (List.length model.firstGroup) Die.rolledDie)
            )

        Reroll ->
            { model | rerolls = model.rerolls - 1 }
                |> update Roll

        NewDice newDice ->
            case model.phase of
                One ->
                    ( { model
                        | firstGroup = newDice
                        , score = getTotal newDice
                      }
                    , Cmd.none
                    )

                Two ->
                    ( { model | secondGroup = newDice }
                    , Cmd.none
                    )

        FreezeOne ->
            ( { model | phase = Two }
            , Cmd.none
            )

        FreezeTwo ->
            ( { model | phase = Two }
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
        [ div []
            [ Html.text ("Score:" ++ String.fromInt model.score)
            , Html.text ("Rerolls: " ++ String.fromInt model.rerolls)
            ]
        , div []
            (List.append
                (List.map Die.toSvg model.firstGroup)
                [ br [] []
                , button [ onClick (rollOrReroll model) ] [ Html.text "Roll" ]
                , button [ onClick FreezeOne ] [ Html.text "Freeze" ]
                ]
            )
        , div []
            (List.append
                (List.map Die.toSvg model.secondGroup)
                [ br [] []
                , button [ onClick (rollOrReroll model) ] [ Html.text "Roll" ]
                , button [ onClick FreezeTwo ] [ Html.text "Freeze" ]
                ]
            )
        ]


getTotal : List Die.Die -> Int
getTotal dice =
    List.sum (List.map Die.asInt dice)


rollOrReroll : Model -> Msg
rollOrReroll model =
    let
        firstDieVal =
            model.firstGroup
                |> List.head
                |> Maybe.withDefault Die.blank
                |> Die.asInt

        secondDieVal =
            model.secondGroup
                |> List.head
                |> Maybe.withDefault Die.blank
                |> Die.asInt

        phase =
            model.phase
    in
    case ( phase, firstDieVal, secondDieVal ) of
        ( One, 0, _ ) ->
            Roll

        ( Two, _, 0 ) ->
            Roll

        ( _, _, _ ) ->
            Reroll
