module App.Square where

import Prelude

data Square =
    Unrevealed
    | Revealed Int
    | Flagged
    | Mine