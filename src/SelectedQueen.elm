module SelectedQueen exposing (SelectedQueen(..))

import Queen exposing(Queen)

type SelectedQueen
    = Available Queen
    | Placed Queen
    | NothingSelected
