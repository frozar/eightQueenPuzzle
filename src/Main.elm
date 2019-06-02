module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Debug
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Queen =
    String


type alias Position =
    ( Int, Int )


type alias Board =
    Array (Array (Maybe Queen))


type SelectedQueen
    = Available Queen
    | Placed Queen
    | NothingSelected


type alias Model =
    { selectedQueen : SelectedQueen
    , queens : List Queen
    , board : Board
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


initBoard : Board
initBoard =
    Array.repeat nbPiece (Array.repeat nbPiece Nothing)


init : Model
init =
    { selectedQueen = NothingSelected
    , queens = initQueens
    , board = initBoard
    , queenAgainst = []
    }



-- UPDATE


type Msg
    = Select Queen
    | SelectCell Int Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select queen ->
            case model.selectedQueen of
                Available selectedAvailableQueen ->
                    if queen == selectedAvailableQueen then
                        { model
                            | selectedQueen = NothingSelected
                        }

                    else
                        { model
                            | selectedQueen = Available queen
                        }

                Placed selectedPlacedQueen ->
                    { model
                        | selectedQueen = Available queen
                    }

                NothingSelected ->
                    { model
                        | selectedQueen = Available queen
                    }

        SelectCell i j ->
            case hasQueenAt model.board i j of
                -- If a queen is selected on the board
                Just queen ->
                    case model.selectedQueen of
                        Placed alreadySelectedQueen ->
                            -- If it is the double click on a placed queen
                            if alreadySelectedQueen == queen then
                                let
                                    updatedBoard =
                                        removeQueenBoard model.board queen
                                in
                                { model
                                    | board = updatedBoard
                                    , queens = alreadySelectedQueen :: model.queens
                                    , selectedQueen = NothingSelected
                                    , queenAgainst = updateQueenAgainst updatedBoard
                                }
                                -- If the click is on another queen

                            else
                                { model
                                    | selectedQueen = Placed queen
                                }

                        Available alreadySelectedQueen ->
                            { model
                                | selectedQueen = Placed queen
                            }

                        NothingSelected ->
                            { model
                                | selectedQueen = Placed queen
                            }

                -- If an empty cell is selected on the board
                Nothing ->
                    case model.selectedQueen of
                        Available queen ->
                            let
                                updatedBoard =
                                    placeQueen model.board queen i j
                            in
                            { model
                                | queens = removeQueenAvailable queen model.queens
                                , board = updatedBoard
                                , selectedQueen = NothingSelected
                                , queenAgainst = updateQueenAgainst updatedBoard
                            }

                        Placed queen ->
                            let
                                updatedBoard =
                                    placeQueen model.board queen i j
                            in
                            { model
                                | board = updatedBoard
                                , selectedQueen = NothingSelected
                                , queenAgainst = updateQueenAgainst updatedBoard
                            }

                        NothingSelected ->
                            model

        Reset ->
            { model
                | selectedQueen = NothingSelected
                , queens = initQueens
                , board = initBoard
                , queenAgainst = []
            }


arrayDrop : Int -> Array a -> Array a
arrayDrop n array =
    Array.slice n (Array.length array) array


getQueenPositionRow : Int -> Int -> Array (Maybe Queen) -> List Position -> List Position
getQueenPositionRow i j maybeQueens listPosition =
    case Array.get 0 maybeQueens of
        Just maybeQueen ->
            case maybeQueen of
                Just queen ->
                    getQueenPositionRow
                        i
                        (j + 1)
                        (arrayDrop 1 maybeQueens)
                        (( i, j ) :: listPosition)

                Nothing ->
                    getQueenPositionRow
                        i
                        (j + 1)
                        (arrayDrop 1 maybeQueens)
                        listPosition

        Nothing ->
            listPosition


getQueenPositionBoard : Int -> Array (Array (Maybe Queen)) -> List Position -> List Position
getQueenPositionBoard i board listPosition =
    case Array.get 0 board of
        Just maybeQueens ->
            getQueenPositionBoard
                (i + 1)
                (arrayDrop 1 board)
                (getQueenPositionRow i 0 maybeQueens listPosition)

        Nothing ->
            listPosition


getPlacedQueens : Board -> List Position
getPlacedQueens board =
    getQueenPositionBoard 0 board []


areAgainst : Position -> Position -> Bool
areAgainst ( i0, j0 ) ( i1, j1 ) =
    (i0 == i1)
        || (j0 == j1)
        || (abs (i0 - i1) == abs (j0 - j1))


getQueenAgainst : List Position -> List ( Position, Position ) -> List ( Position, Position )
getQueenAgainst listQueenPosition res =
    case List.head listQueenPosition of
        Just ( i, j ) ->
            let
                listQueenAgainst =
                    List.filterMap
                        (\pos ->
                            if areAgainst ( i, j ) pos then
                                Just ( ( i, j ), pos )

                            else
                                Nothing
                        )
                        (List.drop 1 listQueenPosition)
            in
            getQueenAgainst
                (List.drop 1 listQueenPosition)
                (List.append listQueenAgainst res)

        Nothing ->
            res


updateQueenAgainst : Board -> List ( Position, Position )
updateQueenAgainst board =
    getQueenAgainst (getPlacedQueens board) []


hasQueenAt : Board -> Int -> Int -> Maybe Queen
hasQueenAt board iPlace jPlace =
    case Array.get iPlace board of
        Just arrayQueen ->
            case Array.get jPlace arrayQueen of
                Just maybeQueen ->
                    maybeQueen

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


removeQueenBoard : Board -> Queen -> Board
removeQueenBoard board queen =
    Array.map
        (\arrayQueen ->
            Array.map
                (\maybeQueen ->
                    case maybeQueen of
                        Just q ->
                            if q == queen then
                                Nothing

                            else
                                Just q

                        Nothing ->
                            Nothing
                )
                arrayQueen
        )
        board


placeQueen : Board -> Queen -> Int -> Int -> Board
placeQueen board queen iPlace jPlace =
    Array.indexedMap
        (\i arrayQueen ->
            Array.indexedMap
                (\j maybeQueen ->
                    if (i == iPlace) && (j == jPlace) then
                        Just queen

                    else
                        case maybeQueen of
                            Just q ->
                                if q == queen then
                                    Nothing

                                else
                                    Just q

                            Nothing ->
                                Nothing
                )
                arrayQueen
        )
        board


removeQueenAvailable : Queen -> List Queen -> List Queen
removeQueenAvailable queen listQueen =
    List.filter (\q -> q /= queen) listQueen


isSelected : SelectedQueen -> Queen -> Bool
isSelected selectedQueen q0 =
    case selectedQueen of
        Available q1 ->
            q0 == q1

        Placed q1 ->
            q0 == q1

        NothingSelected ->
            False



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "text-align" "center"
        ]
        [ h1 [] [ text "8 Queens Puzzle" ]
        , viewBoard model.board model.selectedQueen model.queenAgainst
        , div []
            [ span [] (List.map (viewQueen model.selectedQueen) model.queens)
            ]
        , div []
            [ div [] [ viewVictory model ] ]
        , div []
            [ button [ onClick Reset ] [ text "RAZ" ] ]
        ]


viewVictory : Model -> Html Msg
viewVictory model =
    if List.isEmpty model.queens && List.isEmpty model.queenAgainst then
        text "VICTORY"

    else
        text ""


viewBoard : Board -> SelectedQueen -> List ( Position, Position ) -> Html Msg
viewBoard board selectedQueen queenAgainst =
    Html.table
        [ style "margin-left" "auto"
        , style "margin-right" "auto"
        ]
        [ Html.tbody
            []
            (Array.toList
                (Array.indexedMap
                    (\i arrayQueen ->
                        Html.tr []
                            (Array.toList
                                (Array.indexedMap
                                    (\j maybeQueen ->
                                        viewBoardCell selectedQueen queenAgainst maybeQueen i j
                                    )
                                    arrayQueen
                                )
                            )
                    )
                    board
                )
            )
        ]


isCellBetweenAgainstQueen : Position -> ( Position, Position ) -> Bool
isCellBetweenAgainstQueen ( i, j ) ( ( i0, j0 ), ( i1, j1 ) ) =
    if i0 == i1 then
        (i == i0)
            && (((j0 < j) && (j < j1))
                    || ((j1 < j) && (j < j0))
               )

    else if j0 == j1 then
        (j == j0)
            && (((i0 < i) && (i < i1))
                    || ((i1 < i) && (i < i0))
               )

    else if (0 < (i1 - i0)) && (0 < (j1 - j0)) && (i1 - i0) == (j1 - j0) then
        -- Diagonal lower - right
        (i0 < i) && (i < i1) && (j0 < j) && (j < j1) && ((i - i0) == (j - j0))

    else if (0 < (i1 - i0)) && (0 < (j0 - j1)) && (i1 - i0) == (j0 - j1) then
        -- Diagonal upper - right
        (i0 < i) && (i < i1) && (j1 < j) && (j < j0) && ((i - i0) == (j - j1))

    else if (0 < (i0 - i1)) && (0 < (j1 - j0)) && (i0 - i1) == (j1 - j0) then
        -- Diagonal lower - left
        (i1 < i) && (i < i0) && (j0 < j) && (j < j1) && ((i - i1) == (j - j0))

    else if (0 < (i0 - i1)) && (0 < (j0 - j1)) && (i0 - i1) == (j0 - j1) then
        -- Diagonal upper - left
        (i1 < i) && (i < i0) && (j1 < j) && (j < j0) && ((i - i1) == (j - j1))

    else
        False


isCellBetweenAgainstQueens : Position -> List ( Position, Position ) -> Bool
isCellBetweenAgainstQueens ( i, j ) queenAgainst =
    List.foldr
        (\coupleQueen res -> res || isCellBetweenAgainstQueen ( i, j ) coupleQueen)
        False
        queenAgainst


viewBoardCell : SelectedQueen -> List ( Position, Position ) -> Maybe Queen -> Int -> Int -> Html Msg
viewBoardCell selectedQueen queenAgainst maybeQueen i j =
    Html.td
        [ style "border" "1px solid #505050"
        , style "text-align" "center"
        , style "padding" "0px"
        , style "width" "2rem"
        , style "height" "2rem"
        , style "background-color"
            (if modBy 2 (i + j) == 0 then
                "black"

             else
                "white"
            )
        , onClick (SelectCell i j)
        ]
        [ case maybeQueen of
            Just queen ->
                span
                    [ style "color"
                        (if isSelected selectedQueen queen then
                            "red"

                         else if modBy 2 (i + j) == 0 then
                            "white"

                         else
                            "black"
                        )
                    ]
                    [ text queen ]

            Nothing ->
                span
                    [ style "color"
                        (if modBy 2 (i + j) == 0 then
                            "white"

                         else
                            "black"
                        )
                    ]
                    [ if isCellBetweenAgainstQueens ( i, j ) queenAgainst then
                        text "X"

                      else
                        text ""
                    ]
        ]


viewQueen : SelectedQueen -> Queen -> Html Msg
viewQueen selectedQueen queen =
    span
        [ onClick (Select queen)
        , style "padding" "0.45em"

        -- , style "width" "50px"
        , style "color"
            (if isSelected selectedQueen queen then
                "red"

             else
                "black"
            )
        ]
        [ text queen ]
