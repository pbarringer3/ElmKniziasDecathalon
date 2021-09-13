module HundredMeters exposing (main)

import Browser
import Die
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , column
        , download
        , el
        , fill
        , layout
        , padding
        , paragraph
        , row
        , text
        , textColumn
        , width
        )
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Html
import List
import Random
import String exposing (fromInt)



-- import Svg
-- import Svg.Attributes
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
        (column [ width fill, alignLeft ]
            [ viewHeader model.rerolls, viewContent model ]
        )


viewHeader : Int -> Element Msg
viewHeader rerolls =
    row [ width fill, padding 10, alignLeft ]
        [ text ("Rerolls Remaining: " ++ fromInt rerolls)
        , el [ alignRight, Events.onClick ToggleRules ] (text "Rules")
        ]


viewContent : Model -> Element Msg
viewContent model =
    case model.rulesExpanded of
        True ->
            row [] [ viewGame model, viewRules ]

        False ->
            row [] [ viewGame model ]


viewGame : Model -> Element Msg
viewGame model =
    column []
        [ viewPhase model One
        , viewPhase model Two
        , el [] (text ("Total: " ++ fromInt (model.score1 + model.score2)))
        ]


viewRules : Element msg
viewRules =
    textColumn []
        [ el
            [ Font.size 18
            , Font.bold
            ]
            (text "100 Metres (8 dice, 1 attempt)")
        , el [ Font.bold ] (text "Rules:")
        , paragraph []
            [ text """Divide the eight dice into two sets of four. 
            Roll the first four dice. If you are not satisfied 
            with the result, pick up all four dice and reroll 
            them. This can be repeated several times until you 
            freeze the first set. Then roll the other four dice 
            and proceed in the same manner. Try to freeze sets 
            of dice with high values but which contain no sixes.""" ]
        , paragraph []
            [ text """You have a maximum of seven rolls, one initial 
            roll for each set and up to five rerolls which may 
            be divided between the sets as desired.""" ]
        , el [ Font.bold ] (text "Scoring:")
        , paragraph []
            [ text """Total the value of the dice for numbers one to 
            five, but subtract any sixes from the result.""" ]
        , paragraph [ Font.italic ]
            [ text """Rules lightly adapted from """
            , download []
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
    row []
        [ viewButtons rollType rollEnabled freezeEnabled
        , viewDice dice
        , el [] (text ("Value: " ++ fromInt phaseScore))
        ]


viewButtons : Msg -> Bool -> Bool -> Element Msg
viewButtons rollType rollEnabled freezeEnabled =
    let
        rollButton =
            if rollEnabled then
                button [] { onPress = Just rollType, label = text "Roll" }

            else
                el [] (text "")

        freezeButton =
            if freezeEnabled then
                button [] { onPress = Just Freeze, label = text "Freeze" }

            else
                el [] (text "")
    in
    column []
        [ rollButton
        , freezeButton
        ]


viewDice dice =
    row []
        (dice
            |> List.map (Die.toSvg 80)
            |> List.map Element.html
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
