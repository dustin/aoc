{-|
Module      : AoC
Description : Common stuff for AoC
Copyright   : (c) Dustin Sallings, 2018
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Common stuff across AoC solutions.
-}
module Advent.AoC (
  -- * Parsing
  Parser, parseFile, parseLit, parseGrid,
  -- * Distances
  mdist2, mdist3, mdist4,
  -- * Tuple Helpers
  fst3, snd3, thrd,
  zipt2, zipt3, zipt4,
  ftup, ftup3, ftup4,
  zipWith2D,
  -- * Going around in circles
  succ', pred',
  ntimes,
  -- * Selection
  select,
  -- * Strange Loops
  möb, löb
  ) where

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import qualified Data.Text.IO          as TIO
import           Data.Void             (Void)
import           Text.Megaparsec       (Parsec, parse)
import           Text.Megaparsec.Error (errorBundlePretty)

-- | A Megaparsec Parser.
type Parser = Parsec Void Text

-- | Load a file (e.g. "input/day24") with the given parser.
parseFile :: Parser a -> String -> IO a
parseFile f s = TIO.readFile s >>= either (fail . errorBundlePretty) pure . parse f s

-- | Parse a literal example.
parseLit :: Parser a -> Text -> a
parseLit f s = either (error.errorBundlePretty) id (parse f "" s)

-- | Parallel application of a function across elements of a tuple.
zipt2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipt2 f (a1,b1) (a2,b2) = (f a1 a2, f b1 b2)

-- | Parallel application of a function across elements of a tuple.
zipt3 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
zipt3 f (a1,b1,c1) (a2,b2,c2) = (f a1 a2, f b1 b2, f c1 c2)

-- | Parallel application of a function across elements of a tuple.
zipt4 :: (a -> b -> c) -> (a,a,a,a) -> (b,b,b,b) -> (c,c,c,c)
zipt4 f (a1,b1,c1,d1) (a2,b2,c2,d2) = (f a1 a2, f b1 b2, f c1 c2, f d1 d2)

-- | fmap for uniform tuples.
ftup :: (a -> b) -> (a,a) -> (b,b)
ftup f (a,b) = (f a, f b)
{-# INLINE ftup #-}

-- | fmap for uniform tuples
ftup3 :: (a -> b) -> (a,a,a) -> (b,b,b)
ftup3 f (a,b,c) = (f a, f b, f c)
{-# INLINE ftup3 #-}

-- | fmap for uniform tuples
ftup4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
ftup4 f (a,b,c,d) = (f a, f b, f c, f d)
{-# INLINE ftup4 #-}

-- | Two dimensional manhattan distance.
mdist2 :: (Int,Int) -> (Int,Int) -> Int
mdist2 as bs = let (a,b) = zipt2 mdist1 as bs in a+b

-- | Three dimensional manhattan distance.
mdist3 :: (Int,Int,Int) -> (Int,Int,Int) -> Int
mdist3 as bs = let (a,b,c) = zipt3 mdist1 as bs in a+b+c

-- | Four dimensional manhattan distance.
mdist4 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
mdist4 as bs = let (a,b,c,d) = zipt4 mdist1 as bs in a+b+c+d

-- | One dimensional manhattan distance (for combining with the above).
mdist1 :: Int -> Int -> Int
mdist1 a b = abs (a - b)

-- | A circular succ
succ' :: (Bounded a, Enum a, Eq a) => a -> a
succ' a
  | a == maxBound = minBound
  | otherwise = succ a

-- | A circular pred
pred' :: (Bounded a, Enum a, Eq a) => a -> a
pred' a
  | a == minBound = maxBound
  | otherwise = pred a

-- | zipWith, but along a two-dimensional input.
zipWith2D :: (x -> y -> a -> r) -> [x] -> [y] -> [[a]] -> [r]
zipWith2D f xs ys = foldMap (\(y, l) -> zipWith (`f` y) xs l) . zip ys

-- | Parse a grid into a map of (x,y) pairs.
parseGrid :: (Char -> a) -> String -> Map (Int,Int) a
parseGrid f = Map.fromList . zipWith2D (\x y a -> ((x,y), f a)) [0..] [0..] . lines

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thrd :: (a,b,c) -> c
thrd (_,_,c) = c

-- | Repeat an operation n times.
ntimes :: Int -> (a -> a) -> a -> a
ntimes n f a = iterate f a !! n

-- | expand a list to all of the variants of the list with an element removed.
--
-- > select "cat"  ->  [('c',"at"),('a',"ct"),('t',"ca")]
select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = [(x,xs)] <> (fmap (x:) <$> select xs)

-- | Löb's theorem.
-- https://en.wikipedia.org/wiki/L%C3%B6b%27s_theorem
-- https://github.com/quchen/articles/blob/master/loeb-moeb.md
möb :: (((a -> b) -> b) -> c -> a) -> c -> a
möb f x = go where go = f ($ go) x

-- | Löb's theorem.
-- https://en.wikipedia.org/wiki/L%C3%B6b%27s_theorem
-- https://github.com/quchen/articles/blob/master/loeb-moeb.md
löb :: Functor f => f (f a -> a) -> f a
löb = möb fmap
