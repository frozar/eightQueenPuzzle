module Msg exposing (Msg(..))

import Queen


type Msg
    = Select Queen.Queen
    | SelectCell Int Int
    | Reset
