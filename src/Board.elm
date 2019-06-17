module Board exposing (Board, hasQueenAt, init, isSelected, placeQueen, removeQueen, updateQueenAgainst, viewBoard)

import Array exposing (Array)
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Queen exposing (Queen)
import SelectedQueen exposing (SelectedQueen)



-- type alias Queen =
--     String


type alias Position =
    ( Int, Int )


type alias Board =
    Array (Array (Maybe Queen))



-- type SelectedQueen
--     = Available Queen
--     | Placed Queen
--     | NothingSelected


init : Int -> Board
init nbPiece =
    Array.repeat nbPiece (Array.repeat nbPiece Nothing)


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


removeQueen : Board -> Queen -> Board
removeQueen board queen =
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


getQueenPosition : Int -> Array (Array (Maybe Queen)) -> List Position -> List Position
getQueenPosition i board listPosition =
    case Array.get 0 board of
        Just maybeQueens ->
            getQueenPosition
                (i + 1)
                (arrayDrop 1 board)
                (getQueenPositionRow i 0 maybeQueens listPosition)

        Nothing ->
            listPosition


getPlacedQueens : Board -> List Position
getPlacedQueens board =
    getQueenPosition 0 board []


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



-- UPDATE
-- type Msg
--     = SelectCell Int Int
-- VIEW


viewBoard : Board -> SelectedQueen -> List ( Position, Position ) -> Html.Html Msg
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
        -- Diagonal down - right
        (i0 < i) && (i < i1) && (j0 < j) && (j < j1) && ((i - i0) == (j - j0))

    else if (0 < (i1 - i0)) && (0 < (j0 - j1)) && (i1 - i0) == (j0 - j1) then
        -- Diagonal down - left
        (i0 < i) && (i < i1) && (j1 < j) && (j < j0) && ((i - i0) == (j0 - j))

    else if (0 < (i0 - i1)) && (0 < (j1 - j0)) && (i0 - i1) == (j1 - j0) then
        -- Diagonal up - right
        (i1 < i) && (i < i0) && (j0 < j) && (j < j1) && ((i - i1) == (j1 - j))

    else if (0 < (i0 - i1)) && (0 < (j0 - j1)) && (i0 - i1) == (j0 - j1) then
        -- Diagonal up - left
        (i1 < i) && (i < i0) && (j1 < j) && (j < j0) && ((i - i1) == (j - j1))

    else
        False


isCellBetweenAgainstQueens : Position -> List ( Position, Position ) -> Bool
isCellBetweenAgainstQueens ( i, j ) queenAgainst =
    List.foldr
        -- (\coupleQueen res -> res || isCellBetweenAgainstQueen ( i, j ) (Debug.log "coupleQueen" coupleQueen))
        (\coupleQueen res -> res || isCellBetweenAgainstQueen ( i, j ) coupleQueen)
        False
        queenAgainst


isSelected : SelectedQueen -> Queen -> Bool
isSelected selectedQueen q0 =
    case selectedQueen of
        SelectedQueen.Available q1 ->
            q0 == q1

        SelectedQueen.Placed q1 ->
            q0 == q1

        SelectedQueen.NothingSelected ->
            False


viewBoardCell : SelectedQueen -> List ( Position, Position ) -> Maybe Queen -> Int -> Int -> Html.Html Msg
viewBoardCell selectedQueen queenAgainst maybeQueen i j =
    Html.td
        [ style "border" "1px solid #505050"
        , style "text-align" "center"
        , style "padding" "0px"
        , style "width" "4rem"
        , style "height" "4rem"
        , style "background-color"
            (if modBy 2 (i + j) == 0 then
                "black"

             else
                "white"
            )
        , onClick (Msg.SelectCell i j)
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
