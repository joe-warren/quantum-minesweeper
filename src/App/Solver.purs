module App.Solver where

import Prelude

import Data.Maybe
import App.Grid (Grid)
import App.Grid as Grid
import Data.Foldable (sum)

data State = 
    Constraint Int
    | Undecided
    | Full
