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

-- | Diagonal neighbors.
diags :: Point -> [Point]
diags (x,y) = [(x-1,y-1), (x-1,y+1), (x+1,y+1), (x+1,y-1)]

-- | Get the points around a point (including diagonals).
aroundD :: Point -> [Point]
aroundD (x,y) = [(x+xo, y+yo) | xo <- [-1..1], yo <- [-1..1], xo /= 0 || yo /= 0]

-- | Directions.
data Dir = N | E | S | W deriving (Show, Bounded, Enum, Eq)

-- | Move a point one position in a particular direction.
fwd :: Dir -> Point -> Point
fwd = fwdBy 1

-- | Move a point some amount in a particular direction.
fwdBy :: Int -> Dir -> Point -> Point
fwdBy n N = addPoint (0, n)
fwdBy n S = addPoint (0, -n)
fwdBy n W = addPoint (-n, 0)
fwdBy n E = addPoint (n, 0)

-- | Add some to points.
addPoint :: (Int,Int) -> Point -> Point
addPoint (dx, dy) (x, y) = (dx+x,dy+y)

-- | Multiply x and y values in a point by the given amounts.
mulPoint :: (Int,Int) -> Point -> Point
mulPoint (dx, dy) (x, y) = (dx*x,dy*y)
