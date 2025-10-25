module App.Solver
  ( Constraint
  , Region(..)
  , initialSolveState
  )
  where

import Prelude

import Data.Maybe
import App.Grid (Grid)
import App.Grid as Grid
import App.Square (Square)
import App.Square as Square
import Data.Foldable (sum)
import Data.Array.ST as STArray
import Data.Array.ST (STArray)
import Data.Map as Map
import Data.Map (Map)
import Data.Set as Set
import Data.Set (Set)
import Control.Monad.ST (ST)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)

newtype Region= Region (Set Grid.Coordinates)

derive instance Eq Region
derive instance Ord Region

type Constraint =
    { constraintPos :: Grid.Coordinates
    , constraintCount :: Int
    , constraintRegions :: Set Region
    }

type SolveState s =
    { constraints :: Set Constraint
    , regions :: Map Region (STArray s Int)
    }

initialSolveState :: forall s. Grid Square -> ST s (SolveState s)
initialSolveState g =
    let squareRegion c = 
            let lookup c' = 
                    case Grid.index g c' of
                        (Just (Square.Revealed i)) -> Set.singleton c'
                        _ -> Set.empty
            in foldMap lookup (Grid.neighbours c)
        allRegions =
            let lookup _ (Square.Revealed _) = Set.empty
                lookup c _ = Set.singleton (squareRegion c)
            in foldMapWithIndex lookup g
    in do
        pure
            { constraints: mempty
            , regions: Map.empty
            }

