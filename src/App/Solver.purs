module App.Solver
  ( Constraint
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
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Array.ST (STArray)
import Data.Map as Map
import Data.Map (Map)
import Data.Set as Set
import Data.Set (Set)
import Control.Monad.ST (ST)
import Data.Tuple (Tuple (..))
import Data.Traversable (traverse, sequence)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)


type Constraint =
    { constraintPos :: Grid.Coordinates
    , constraintCount :: Int
    , constraintRegions :: Set (Set Grid.Coordinates)
    }

type SolveState s =
    { constraints :: Set Constraint
    , regions :: Map (Set Grid.Coordinates) (STArray s Int)
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
        allConstraints = 
            let lookup c (Square.Revealed i) =
                    Set.singleton 
                        { constraintPos: c
                        , constraintCount:  i
                        , constraintRegions: Set.filter (Set.member c) allRegions 
                        }
                lookup _ _ = Set.empty
            in foldMapWithIndex lookup g
        newArray i = STArray.thaw (Array.range 0 i)
    in do
        regions <- traverse newArray <<< Map.fromFoldableWith (+) <<< map (\i -> (Tuple i 1)) <<< Array.fromFoldable $ allRegions
        pure
            { constraints: allConstraints
            , regions: regions
            }

