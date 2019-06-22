module Queen exposing (Msg(..), Queen, SelectedQueen(..), isSelected, view)

import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Queen =
    String


type SelectedQueen
    = Available Queen
    | Placed Queen
    | NothingSelected


type Msg
    = Select Queen
    | SelectCell Int Int
    | Reset



-- VIEW


isSelected : SelectedQueen -> Queen -> Bool
isSelected selectedQueen q0 =
    case selectedQueen of
        Available q1 ->
            q0 == q1

        Placed q1 ->
            q0 == q1

        NothingSelected ->
            False


view : SelectedQueen -> Queen -> Html Msg
view selectedQueen queen =
    span
        [ onClick (Select queen)
        , style "padding" "0.45em"
        , style "color"
            (if isSelected selectedQueen queen then
                "red"

             else
                "black"
            )
        ]
        [ text queen ]
