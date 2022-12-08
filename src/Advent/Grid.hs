{-|
Module      : Advent.Grid
Description : Accessing a rectangular file as (Int,Int) -> Word8
Copyright   : (c) Dustin Sallings, 2021
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

This is useful any time you've got a map of x,y coordinates you want
to fetch values from without having to put everything into a data
structure.  Lookups are O(1), storage is approximately the number of
bytes of the input, etc...
-}

{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}

module Advent.Grid (
  Grid,
  readFile, parseInput,
  bounds, range, index, inRange,
  lookup, unsafeLookup, assocs, assocsMap, b2c,
  map, ixmap
  ) where

import           Advent.TwoD            (Point)
import           Advent.Vis             (Bounded2D (..))
import           Control.DeepSeq        (NFData (..))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Ix                as Ix
import           Data.Tuple             (swap)
import           Data.Word              (Word8)
import           GHC.Base               (unsafeChr)
import           GHC.Generics           (Generic)
import           Prelude                hiding (readFile, lookup, map)

-- | Grid represents byte data from a file with equal length lines of
-- newline separated data.
data Grid = Grid {
  _bounds  :: !(Point, Point),
  _vBounds :: !(Point, Point),
  _bytes   :: !BS.ByteString
  } deriving Generic

instance NFData Grid

instance Bounded2D Grid where
  bounds2d  = bounds

-- | Return the bounds of this 'Grid'.
bounds :: Grid -> (Point, Point)
bounds = _vBounds
{-# INLINE bounds #-}

-- | Produce a list of all (x,y) 'Point's within this 'Grid'.
range :: Grid -> [Point]
range = Ix.range . Advent.Grid.bounds
{-# INLINE range #-}

-- | Return the numeric index of a point within this 'Grid'.
index :: Grid -> Point -> Int
index Grid{_bounds} = Ix.index _bounds . swap
{-# INLINE index #-}

-- | Return True if the given 'Point' is within this 'Grid'.
inRange :: Grid -> Point -> Bool
inRange Grid{_vBounds} = Ix.inRange _vBounds . swap
{-# INLINE inRange #-}

-- | Load a 'Grid' from a given file path.
--
-- There's not a lot of checking here.  Use this on proper grid data.
readFile :: FilePath -> IO Grid
readFile = fmap parseInput . BS.readFile

-- | Parse a 'ByteString' into a 'Grid'.
--
-- There's not a lot of checking here.  Use this on proper grid data.
parseInput :: BS.ByteString -> Grid
parseInput b = let (x:xs) = BS.elemIndices 10 b in
                   Grid ((0,0),(length xs,x)) ((0,0),(x - 1, length xs)) b

-- | Get the value at the given 'Point' from this 'Grid' iff it's within range.
lookup :: Grid -> Point -> Maybe Word8
lookup (Grid b _ bs) (swap -> p)
  | Ix.inRange b p = Just (BS.unsafeIndex bs (Ix.index b p))
  | otherwise = Nothing
{-# INLINE lookup #-}

-- | Get the value at the given 'Point' from this 'Grid'.
unsafeLookup :: Grid -> Point -> Word8
unsafeLookup (Grid b _ bs) (swap -> p) = BS.unsafeIndex bs (Ix.index b p)
{-# INLINE unsafeLookup #-}

-- | Return all of the 'Point's and values from this 'Grid'.
assocs :: Grid -> [(Point, Word8)]
assocs = assocsMap id

-- | Return all of the 'Point's and values mapped to a particular value. from this 'Grid'.
assocsMap :: (Word8 -> a) -> Grid -> [(Point, a)]
assocsMap f (Grid b@(_,(_,mx)) _ bs) = (\p -> (swap p, f (BS.unsafeIndex bs (Ix.index b p)))) <$> filter noNL (Ix.range b)
  where
    noNL (_,x) = x /= mx

-- | Convert a 'Word8' to a 'Char' in a fairly unsafe but quick way.
b2c :: Word8 -> Char
b2c = unsafeChr . fromIntegral
{-# INLINE b2c #-}

-- | Map all the values in the grid.
map :: (Word8 -> Word8) -> Grid -> Grid
map f g = g{_bytes = BS.map f (_bytes g)}

-- | Map all of the values from the grid including the index.
ixmap :: (Point -> Word8 -> Word8) -> Grid -> Grid
ixmap f grid@(Grid b@(_,(_,mx)) _ bs) = grid{_bytes = snd (BS.mapAccumL g (Ix.range b) bs)}
  where
    g ((y,x):ps) w
      | x == mx = (ps, w)
      | otherwise = (ps, f (x,y) w)
