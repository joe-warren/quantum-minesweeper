module App.Square where

import Prelude

data IsMined = Mined | Unmined

data Square =
    Unrevealed IsMined
    | Revealed Int
    | Flagged IsMined
    | Exploded

isMined :: Square -> IsMined
isMined (Unrevealed x) = x
isMined (Revealed _) = Unmined
isMined (Flagged x) = x
isMined (Exploded) = Mined