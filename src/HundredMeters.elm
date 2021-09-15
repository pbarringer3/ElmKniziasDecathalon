module HundredMeters exposing (main)

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



-- main : Html.Html Msg
-- main =
--     Element.layout []
--         (view (Tuple.first (init ())))
-- MODEL


type alias Model =
    { rerolls : Int
    , firstGroup : List Die.Die
    , secondGroup : List Die.Die
    , phase : Phase
    , score1 : Int
    , score2 : Int
    , rulesExpanded : Bool
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
      , rulesExpanded = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Reroll
    | NewDice (List Die.Die)
    | Freeze
    | ToggleRules


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

        ToggleRules ->
            ( { model | rulesExpanded = not model.rulesExpanded }
            , Cmd.none
            )


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
    layout []
        (column [ width fill ]
            [ viewHeader model.rerolls
            , viewContent model
            ]
        )


viewHeader : Int -> Element Msg
viewHeader rerolls =
    row [ width fill, padding 20, alignLeft ]
        [ text ("Rerolls Remaining: " ++ fromInt rerolls)
        , el [ alignRight, Events.onClick ToggleRules, Font.underline, paddingXY 20 0 ] (text "Rules")
        ]


viewContent : Model -> Element Msg
viewContent model =
    if model.rulesExpanded then
        row [ width fill ]
            [ el [ width (fillPortion 7), alignTop ] (viewGame model)
            , el [ width (fillPortion 3) ] viewRules
            ]

    else
        row [ width fill ] [ viewGame model ]


viewGame : Model -> Element Msg
viewGame model =
    column [ spacing 25, paddingXY 40 0 ]
        [ viewPhase model One
        , viewPhase model Two
        , el [ alignRight ] (text ("Total: " ++ fromInt (model.score1 + model.score2)))
        ]


viewRules : Element msg
viewRules =
    let
        indent =
            paddingEach { top = 0, bottom = 0, right = 0, left = 10 }
    in
    textColumn [ spacing 20, paddingXY 40 0 ]
        [ el
            [ Font.size 24
            , Font.bold
            ]
            (text "100 Metres (8 dice, 1 attempt)")
        , el [ Font.bold ] (text "Rules:")
        , paragraph [ indent ]
            [ text """Divide the eight dice into two sets of four. 
            Roll the first four dice. If you are not satisfied 
            with the result, pick up all four dice and reroll 
            them. This can be repeated several times until you 
            freeze the first set. Then roll the other four dice 
            and proceed in the same manner. Try to freeze sets 
            of dice with high values but which contain no sixes.""" ]
        , paragraph [ indent ]
            [ text """You have a maximum of seven rolls, one initial 
            roll for each set and up to five rerolls which may 
            be divided between the sets as desired.""" ]
        , el [ Font.bold ] (text "Scoring:")
        , paragraph [ indent ]
            [ text """Total the value of the dice for numbers one to 
            five, but subtract any sixes from the result.""" ]
        , paragraph [ Font.italic, Font.size 12, Font.center ]
            [ text """Rules lightly adapted from """
            , download [ Font.underline ]
                { url = """https://www.knizia.de/wp-content/uploads
                    /reiner/freebies/Website-Decathlon.pdf"""
                , label = text "this PDF."
                }
            ]
        ]


viewPhase : Model -> Phase -> Element Msg
viewPhase model phase =
    let
        dice =
            if phase == One then
                model.firstGroup

            else
                model.secondGroup

        phaseScore =
            if phase == One then
                model.score1

            else
                model.score2

        matchedPhases =
            model.phase == phase

        rollType =
            rollOrReroll dice

        rollEnabled =
            matchedPhases && (rollType == Roll || model.rerolls > 0)

        freezeEnabled =
            matchedPhases && (rollType == Reroll)
    in
    row [ spacing 20 ]
        [ column [ spacing 10 ]
            [ viewDice dice
            , el [ centerX ] (viewButtons rollType rollEnabled freezeEnabled)
            ]
        , el [ alignTop, paddingXY 0 40 ] (text ("Value: " ++ fromInt phaseScore))
        ]


viewButtons : Msg -> Bool -> Bool -> Element Msg
viewButtons rollType rollEnabled freezeEnabled =
    let
        buttonAttributes =
            [ Border.width 1, Border.rounded 3, width (px 75), Font.center ]

        buttons =
            [ ( rollEnabled, button buttonAttributes { onPress = Just rollType, label = text "Roll" } )
            , ( freezeEnabled, button buttonAttributes { onPress = Just Freeze, label = text "Freeze" } )
            ]
    in
    row [ spacing 20 ]
        (buttons
            |> List.filter (\tuple -> Tuple.first tuple)
            |> List.map Tuple.second
        )


viewDice : List Die.Die -> Element Msg
viewDice dice =
    row [ spacing 15 ]
        (List.map (Die.toSvgElement 100) dice)


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
