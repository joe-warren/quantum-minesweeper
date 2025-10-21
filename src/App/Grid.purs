module App.Grid where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr, for_, length)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Icons as Icons
import App.Square (Square)
import App.Square as Square

--| This is borrowed from purescript-grid-reactors
data Grid a = Grid (Array a) { width :: Int, height :: Int }

instance Functor Grid where
  map f (Grid xs cfg) = Grid (map f xs) cfg

type Coordinates = { x :: Int, y :: Int }

instance FunctorWithIndex Coordinates Grid where
  mapWithIndex f (Grid xs cfg) = Grid (Array.mapWithIndex (f <<< to2D cfg.width) xs) cfg

instance Foldable Grid where
    foldr f z (Grid xs _) = foldr f z xs
    foldl f z (Grid xs _) = foldl f z xs
    foldMap f (Grid xs _) = foldMap f xs

to1D :: Int -> Coordinates -> Int
to1D width { x, y } = y * width + x

to2D :: Int -> Int -> Coordinates
to2D width i = { x: i `mod` width, y: i / width }

size :: forall a. Grid a -> { width :: Int, height :: Int }
size (Grid _ cfg) = cfg

index :: forall a. Grid a -> Coordinates -> Maybe a
index (Grid xs { height, width }) { x, y }
  | x < 0 || x >= width || y < 0 || y >= height = Nothing
  | otherwise = Array.index xs $ to1D width { x, y }

updateAt' :: forall a. Coordinates -> a -> Grid a -> Grid a
updateAt' coords new g = fromMaybe g $ updateAt coords new g

updateAt :: forall a. Coordinates -> a -> Grid a -> Maybe (Grid a)
updateAt coords new = modifyAt coords (const new)

modifyAt' :: forall a. Coordinates -> (a -> a) -> Grid a -> Grid a
modifyAt' coords f g = fromMaybe g $ modifyAt coords f g

modifyAt :: forall a. Coordinates -> (a -> a) -> Grid a -> Maybe (Grid a)
modifyAt coords f (Grid xs cfg@{ width }) =
  map (\ys -> Grid ys cfg) $
    Array.modifyAt (to1D width coords) f xs

replicate :: forall a. Int -> Int -> a -> Grid a
replicate width height x = Grid (Array.replicate (width * height) x) { width, height }

empty :: Int -> Int -> Grid Square
empty w h = replicate w h Square.Unrevealed