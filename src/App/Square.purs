module App.Square where

import Prelude

data IsMined = Mined | Unmined

derive instance Eq IsMined

data Square =
    Unrevealed IsMined
    | Revealed Int
    | Flagged IsMined
    | Exploded

derive instance Eq Square

isMined :: Square -> IsMined
isMined (Unrevealed x) = x
isMined (Revealed _) = Unmined
isMined (Flagged x) = x
isMined (Exploded) = Mined