module Main exposing (Model, init, main, update, view)

import Array exposing (Array)
import Board
import Browser
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Queen exposing (Msg, Queen, view)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Position =
    ( Int, Int )


type alias Model =
    { selectedQueen : Queen.SelectedQueen
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
    { selectedQueen = Queen.NothingSelected
    , queens = initQueens
    , board = Board.init nbPiece
    , queenAgainst = []
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Queen.Select queen ->
            case model.selectedQueen of
                Queen.Available selectedAvailableQueen ->
                    if queen == selectedAvailableQueen then
                        { model
                            | selectedQueen = Queen.NothingSelected
                        }

                    else
                        { model
                            | selectedQueen = Queen.Available queen
                        }

                Queen.Placed selectedPlacedQueen ->
                    { model
                        | selectedQueen = Queen.Available queen
                    }

                Queen.NothingSelected ->
                    { model
                        | selectedQueen = Queen.Available queen
                    }

        Queen.SelectCell i j ->
            case Board.hasQueenAt model.board i j of
                -- If a queen is selected on the board
                Just queen ->
                    case model.selectedQueen of
                        Queen.Placed alreadySelectedQueen ->
                            -- If it is the double click on a placed queen
                            if alreadySelectedQueen == queen then
                                let
                                    updatedBoard =
                                        Board.removeQueen model.board queen
                                in
                                { model
                                    | board = updatedBoard
                                    , queens = alreadySelectedQueen :: model.queens
                                    , selectedQueen = Queen.NothingSelected
                                    , queenAgainst = Board.updateQueenAgainst updatedBoard
                                }
                                -- If the click is on another queen

                            else
                                { model
                                    | selectedQueen = Queen.Placed queen
                                }

                        Queen.Available alreadySelectedQueen ->
                            { model
                                | selectedQueen = Queen.Placed queen
                            }

                        Queen.NothingSelected ->
                            { model
                                | selectedQueen = Queen.Placed queen
                            }

                -- If an empty cell is selected on the board
                Nothing ->
                    case model.selectedQueen of
                        Queen.Available queen ->
                            let
                                updatedBoard =
                                    Board.placeQueen model.board queen i j
                            in
                            { model
                                | queens = removeQueenAvailable queen model.queens
                                , board = updatedBoard
                                , selectedQueen = Queen.NothingSelected
                                , queenAgainst = Board.updateQueenAgainst updatedBoard
                            }

                        Queen.Placed queen ->
                            let
                                updatedBoard =
                                    Board.placeQueen model.board queen i j
                            in
                            { model
                                | board = updatedBoard
                                , selectedQueen = Queen.NothingSelected
                                , queenAgainst = Board.updateQueenAgainst updatedBoard
                            }

                        Queen.NothingSelected ->
                            model

        Queen.Reset ->
            { model
                | selectedQueen = Queen.NothingSelected
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
            [ span [] (List.map (Queen.view model.selectedQueen) model.queens)
            ]
        , div []
            [ div [] [ viewVictory model ] ]
        , div []
            [ button [ onClick Queen.Reset ] [ text "RAZ" ] ]
        ]


viewVictory : Model -> Html Msg
viewVictory model =
    if List.isEmpty model.queens && List.isEmpty model.queenAgainst then
        text "VICTORY"

    else
        text ""
