module Main exposing (Model, init, main, update, view)

import Array exposing (Array)
import Board
import Browser
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Queen exposing (Queen)
import SelectedQueen exposing (SelectedQueen)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Position =
    ( Int, Int )


type alias Model =
    { selectedQueen : SelectedQueen.SelectedQueen
    , queens : List Queen
    , board : Board.Board
    , queenAgainst : List ( Position, Position )
    }


nbPiece =
    8


noneQueen : Queen
noneQueen =
    "xx"


initQueens : List Queen
initQueens =
    List.indexedMap
        (\i q -> "Q" ++ String.fromInt i)
        (List.repeat 8 noneQueen)


init : Model
init =
    -- { selectedQueen = Board.SelectedQueen.NothingSelected
    { selectedQueen = SelectedQueen.NothingSelected
    , queens = initQueens
    , board = Board.init nbPiece
    , queenAgainst = []
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg.Select queen ->
            case model.selectedQueen of
                SelectedQueen.Available selectedAvailableQueen ->
                    if queen == selectedAvailableQueen then
                        { model
                            | selectedQueen = SelectedQueen.NothingSelected
                        }

                    else
                        { model
                            | selectedQueen = SelectedQueen.Available queen
                        }

                SelectedQueen.Placed selectedPlacedQueen ->
                    { model
                        | selectedQueen = SelectedQueen.Available queen
                    }

                SelectedQueen.NothingSelected ->
                    { model
                        | selectedQueen = SelectedQueen.Available queen
                    }

        Msg.SelectCell i j ->
            case Board.hasQueenAt model.board i j of
                -- If a queen is selected on the board
                Just queen ->
                    case model.selectedQueen of
                        SelectedQueen.Placed alreadySelectedQueen ->
                            -- If it is the double click on a placed queen
                            if alreadySelectedQueen == queen then
                                let
                                    updatedBoard =
                                        Board.removeQueen model.board queen
                                in
                                { model
                                    | board = updatedBoard
                                    , queens = alreadySelectedQueen :: model.queens
                                    , selectedQueen = SelectedQueen.NothingSelected
                                    , queenAgainst = Board.updateQueenAgainst updatedBoard
                                }
                                -- If the click is on another queen

                            else
                                { model
                                    | selectedQueen = SelectedQueen.Placed queen
                                }

                        SelectedQueen.Available alreadySelectedQueen ->
                            { model
                                | selectedQueen = SelectedQueen.Placed queen
                            }

                        SelectedQueen.NothingSelected ->
                            { model
                                | selectedQueen = SelectedQueen.Placed queen
                            }

                -- If an empty cell is selected on the board
                Nothing ->
                    case model.selectedQueen of
                        SelectedQueen.Available queen ->
                            let
                                updatedBoard =
                                    Board.placeQueen model.board queen i j
                            in
                            { model
                                | queens = removeQueenAvailable queen model.queens
                                , board = updatedBoard
                                , selectedQueen = SelectedQueen.NothingSelected
                                , queenAgainst = Board.updateQueenAgainst updatedBoard
                            }

                        SelectedQueen.Placed queen ->
                            let
                                updatedBoard =
                                    Board.placeQueen model.board queen i j
                            in
                            { model
                                | board = updatedBoard
                                , selectedQueen = SelectedQueen.NothingSelected
                                , queenAgainst = Board.updateQueenAgainst updatedBoard
                            }

                        SelectedQueen.NothingSelected ->
                            model

        Msg.Reset ->
            { model
                | selectedQueen = SelectedQueen.NothingSelected
                , queens = initQueens
                , board = Board.init nbPiece
                , queenAgainst = []
            }


removeQueenAvailable : Queen -> List Queen -> List Queen
removeQueenAvailable queen listQueen =
    List.filter (\q -> q /= queen) listQueen



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "text-align" "center"
        ]
        [ h1 [] [ text "8 Queens Puzzle" ]
        , Board.viewBoard model.board model.selectedQueen model.queenAgainst
        , div []
            [ span [] (List.map (viewQueen model.selectedQueen) model.queens)
            ]
        , div []
            [ div [] [ viewVictory model ] ]
        , div []
            [ button [ onClick Msg.Reset ] [ text "RAZ" ] ]
        ]


viewVictory : Model -> Html Msg
viewVictory model =
    if List.isEmpty model.queens && List.isEmpty model.queenAgainst then
        text "VICTORY"

    else
        text ""


viewQueen : SelectedQueen -> Queen -> Html Msg
viewQueen selectedQueen queen =
    span
        [ onClick (Msg.Select queen)
        , style "padding" "0.45em"
        , style "color"
            (if Board.isSelected selectedQueen queen then
                "red"

             else
                "black"
            )
        ]
        [ text queen ]
