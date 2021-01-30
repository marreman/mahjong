module Main exposing (main)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (css, type_, value)
import Html.Styled.Events exposing (onInput)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = toUnstyled << view
        }


type alias Model =
    { winner : String
    , winds : Dict String Int
    }


emptyScore : Dict String Int
emptyScore =
    Dict.empty
        |> Dict.insert "Östan" 0
        |> Dict.insert "Nordan" 0
        |> Dict.insert "Västan" 0
        |> Dict.insert "Sunnan" 0


init : Model
init =
    { winner = ""
    , winds = emptyScore
    }


type Msg
    = ChangeScore String String
    | ChangeWinner String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeScore name scoreString ->
            case String.toInt scoreString of
                Just score ->
                    { model | winds = Dict.insert name score model.winds }

                Nothing ->
                    { model | winds = Dict.insert name 0 model.winds }

        ChangeWinner name ->
            { model | winner = name }


view : Model -> Html Msg
view model =
    let
        headers =
            tr [ css [ fontWeight bold ] ]
                [ td [] []
                , td [] [ text "Mahjong" ]
                , td [] [ text "Poäng" ]
                , td [] [ text "Resultat" ]
                ]

        scores =
            calculateScores model

        winds =
            Dict.map (viewWind model.winner scores) model.winds
                |> Dict.values
    in
    Html.table [ css [ fontFamily monospace ] ]
        (headers :: winds)


calculateScores : Model -> Dict String Int
calculateScores model =
    let
        winnerPoints =
            Dict.get model.winner model.winds
                |> Maybe.withDefault 0

        others =
            Dict.toList model.winds
                |> List.filter (Tuple.first >> (/=) model.winner)
                |> List.sortBy Tuple.second
                |> List.reverse
    in
    List.foldl (\( name, _ ) scores -> take winnerPoints name model.winner scores) emptyScore others
        |> do others


do : List ( String, Int ) -> Dict String Int -> Dict String Int
do winds scores =
    case winds of
        to :: others ->
            let
                transact_ ( from, points ) scores_ =
                    take (Tuple.second to - points) from (Tuple.first to) scores_
            in
            List.foldl transact_ scores others
                |> do others

        _ ->
            scores


take : Int -> String -> String -> Dict String Int -> Dict String Int
take amount from to scores =
    let
        _ =
            Debug.log "take" ( amount, from, to )

        amount_ =
            if from == "Östan" || to == "Östan" then
                amount * 2

            else
                amount
    in
    Dict.update from (Maybe.map (\score -> score - amount_)) scores
        |> Dict.update to (Maybe.map (\score -> score + amount_))


viewWind : String -> Dict String Int -> String -> Int -> Html Msg
viewWind winner scores name points =
    tr []
        [ td [] [ text name ]
        , td []
            [ input
                [ type_ "radio"
                , Attrs.name "winner"
                , value name
                , onInput ChangeWinner
                , Attrs.checked (winner == name)
                ]
                []
            ]
        , td []
            [ input
                [ type_ "number"
                , value
                    (if points == 0 then
                        ""

                     else
                        String.fromInt points
                    )
                , onInput (ChangeScore name)
                , css
                    [ fontFamily monospace
                    , textAlign right
                    , width (px 100)
                    ]
                ]
                []
            ]
        , Dict.get name scores
            |> Maybe.map String.fromInt
            |> Maybe.withDefault ""
            |> text
            |> List.singleton
            |> td [ css [ textAlign right ] ]
        ]
