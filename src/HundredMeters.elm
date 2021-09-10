module HundredMeters exposing (main)

import Browser
import Die
import Html exposing (br, button, div)
import Html.Attributes as A
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
    { rerolls : Int
    , firstGroup : List Die.Die
    , secondGroup : List Die.Die
    , phase : Phase
    , score1 : Int
    , score2 : Int
    }


type Phase
    = One
    | Two
    | GameOver


groupLength : Int
groupLength =
    4


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rerolls = 5
      , firstGroup = List.repeat groupLength Die.blank
      , secondGroup = List.repeat groupLength Die.blank
      , phase = One
      , score1 = 0
      , score2 = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Reroll
    | NewDice (List Die.Die)
    | Freeze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewDice (Random.list groupLength Die.rolledDie)
            )

        Reroll ->
            { model | rerolls = model.rerolls - 1 }
                |> update Roll

        NewDice newDice ->
            case model.phase of
                One ->
                    ( { model
                        | firstGroup = newDice
                        , score1 = getTotal newDice
                      }
                    , Cmd.none
                    )

                Two ->
                    ( { model
                        | secondGroup = newDice
                        , score2 = getTotal newDice
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Freeze ->
            case model.phase of
                One ->
                    ( { model | phase = Two }
                    , Cmd.none
                    )

                Two ->
                    ( { model | phase = GameOver }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


getTotal : List Die.Die -> Int
getTotal dice =
    let
        underSix =
            List.filter (\n -> n < 6) (List.map Die.asInt dice)
    in
    List.sum underSix + (groupLength - List.length underSix) * -6



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        [ div []
            [ Html.text ("Score:" ++ String.fromInt (model.score1 + model.score2))
            , Html.text ("Rerolls: " ++ String.fromInt model.rerolls)
            ]
        , viewDiceSection model.firstGroup model.phase One model.rerolls
        , viewDiceSection model.secondGroup model.phase Two model.rerolls
        ]


viewDiceSection : List Die.Die -> Phase -> Phase -> Int -> Html.Html Msg
viewDiceSection dice modelPhase workingPhase rerolls =
    div []
        (List.append
            (List.map (Die.toSvg 80) dice)
            [ br [] []
            , button
                [ onClick (rollOrReroll dice)
                , A.disabled
                    ((modelPhase /= workingPhase)
                        || (rollOrReroll dice == Reroll && rerolls <= 0)
                    )
                ]
                [ Html.text "Roll" ]
            , button
                [ onClick Freeze
                , A.disabled (modelPhase /= workingPhase)
                ]
                [ Html.text "Freeze" ]
            ]
        )


rollOrReroll : List Die.Die -> Msg
rollOrReroll dice =
    let
        val =
            headDieVal dice
    in
    case val of
        0 ->
            Roll

        _ ->
            Reroll


headDieVal : List Die.Die -> Int
headDieVal dice =
    dice
        |> List.head
        |> Maybe.withDefault Die.blank
        |> Die.asInt
