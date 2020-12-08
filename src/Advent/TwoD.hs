{-|
Module      : TwoD
Description : Functions for working in 2D space.
Copyright   : (c) Dustin Sallings, 2019
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Functions for working in 2D space.
-}
module Advent.TwoD where

-- | A point in 2D space.
type Point = (Int,Int)

-- | Get the points around a point (not including diagonals).
around :: Point -> [Point]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

-- | Get the points around a point (including diagonals).
aroundD :: Point -> [Point]
aroundD (x,y) = [(x+xo, y+yo) | xo <- [-1..1], yo <- [-1..1], xo /= 0 || yo /= 0]

-- | Directions.
data Dir = N | E | S | W deriving (Show, Bounded, Enum, Eq)

-- | Move a point in a particular direction.
fwd :: Dir -> Point -> Point
fwd S (x,y) = (x,y+1)
fwd N (x,y) = (x,y-1)
fwd W (x,y) = (x-1,y)
fwd E (x,y) = (x+1,y)
