{-|
Module      : Search
Description : Searching stuff for AoC.
Copyright   : (c) Dustin Sallings, 2018
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Things I use for searching space in AoC.
-}

{-# LANGUAGE LambdaCase #-}

module Advent.Search (
  -- * Graph searching
  dijkstra', dijkstra, resolveDijkstra,
  bfs, bfsOn,
  -- * Binary searching
  binSearch, autoBinSearch, binSearchM,
  -- * List cycle/repeat detection
  findCycle, findRepeated, findRepeatedOn,
  findMin, findMax, countIf,
  -- * Fitting
  arranger,
  -- * List expansion
  perturb) where

import           Advent.AoC
import qualified Advent.Queue                as Queue
import           Control.Applicative         ((<|>))
import           Control.Parallel.Strategies (parList, rseq, using)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (listToMaybe)
import qualified Data.PQueue.Min             as Q
import qualified Data.Set                    as Set

-- | Get the first repeated element.
findRepeated :: Eq a => [a] -> Maybe a
findRepeated = findRepeatedOn id

-- | Get the first repeated element using a comparator function
findRepeatedOn :: Eq b => (a -> b) -> [a] -> Maybe a
findRepeatedOn f xs = listToMaybe [a | (a,b) <- zip xs (tail xs), f a == f b]

-- | Get the position of the start of the first cycle and the cycle length from a list.
findCycle :: Ord b => (a -> b) -> [a] -> (Int, Int, a)
findCycle f = go 0 mempty
  where
    go _ _ [] = error "findCycle: no inputs"
    go n mem (x:xs) = case Map.lookup t mem of
                        Nothing -> go (n+1) (Map.insert t n mem) xs
                        Just o  -> (o, n - o, x)
      where t = f x

-- | bfs finds all reachable states from a given value and a function
-- to find its neighbors.
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

-- this is based on glguy's thing because he makes a good API.

-- | bfsOn finds all reachable states from a given value and a
-- function to find its neighbors using a representative function.
--
-- The representative function allows a separation between the
-- structure of a state and how it's represented, removing the Ord
-- requirement for the state.
bfsOn ::
  Ord r =>
  (a -> r)   {- ^ representative function -} ->
  (a -> [a]) {- ^ neighbors               -} ->
  a          {- ^ initial state           -} ->
  [a]        {- ^ reachable states        -}
bfsOn rep next start = loop Set.empty (Queue.singleton start)
  where
    loop _ Queue.Empty = []
    loop seen inq
      | Set.member r seen =     loop seen xs
      | otherwise         = x : loop (Set.insert r seen) (Queue.appendList xs $ next x)
      where
        r = rep x
        Just (x, xs) = Queue.pop inq

-- Tests for this are in 2018 Day 22.

-- | 'dijkstra'' uses [Dijkstra's
-- Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to
-- find the costs and links from a starting point to various points on
-- a graph.
--
-- See 'resolveDijkstra' for a means of determining a path from the
-- resulting values.
dijkstra' :: Ord v => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
  -> v -- ^ The starting point.
  -> (v -> Bool) -- ^ Predicate allowing early termination of search (if True).
  -> (v, Map v Int, Map v v) -- ^ (cost from origin to v, links from points to points)
dijkstra' neighbrf start done = go (Q.singleton (0,start)) (Map.singleton start 0) mempty mempty
  where
    go q m l seen
      | Q.null q = (pt, m,l)
      | done pt = (pt, m',l)
      | Set.member pt seen = go odo m l seen
      | otherwise = go (odo <> psd) m' l' (Set.insert pt seen)

      where
        ([(d,pt)], odo) = Q.splitAt 1 q
        moves = filter (\(c,p') -> c+d < Map.findWithDefault (c+d+1) p' m) (neighbrf pt) `using` parList rseq
        m' = Map.union (Map.fromList [(p',c+d) | (c,p') <- moves]) m
        l' = Map.union (Map.fromList [(p',pt) | (_,p') <- moves]) l
        psd = Q.fromList [(d+c,x) | (c,x) <- moves]


-- | Using maps computed by 'dijkstra'', find the cost and path from the
-- start to a destination.
resolveDijkstra :: Ord v => Map v Int -> Map v v -> v -> v -> Maybe (Int,[v])
resolveDijkstra m l start end = case Map.lookup end m of
                                  Nothing   -> Nothing
                                  Just cost -> Just (cost, reverse $ end : go end)
  where
    go pt
      | pt == start = []
      | otherwise = next : go next
      where next = l Map.! pt

-- | 'dijkstra''' uses [Dijkstra's
-- Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to
-- find the lowest cost path to a given destination.
dijkstra :: Ord v => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
         -> v -- ^ The starting point.
         -> (v -> Bool) -- ^ Goal function
         -> Maybe (Int,[v])  -- ^ The cost to the destination, and the path to get there.
dijkstra neighbrf start goal = resolve (dijkstra' neighbrf start goal)
  where resolve (e, m,l) = resolveDijkstra m l start e

-- | 'binSearch' performs a binary search to find the boundary
-- function where a function returns its highest 'LT' value.
binSearch :: Integral a => (a -> Ordering) -> a -> a -> a
binSearch f l h
  | h < l     = l
  | v == GT   = binSearch f l (mid-1)
  | v == LT   = binSearch f (mid+1) h
  | otherwise = mid
  where
    mid = l + (h-l) `div` 2
    v = f mid

-- | A binary search with auto-discovering bounds.
autoBinSearch :: Integral a => (a -> Ordering) -> a
autoBinSearch f = go 0 0 (if dir == LT then 1 else -1)
  where
    dir = f 0
    go p l o
      | v == EQ = l
      | v == dir = go l (l + o) (o * 10)
      | otherwise = binSearch f (min p l) (max p l)
      where v = f l

-- | 'binSearchM' performs a binary search over a monadic action to
-- find the boundary function where a function returns its highest
-- 'LT' value.
binSearchM :: (Integral a, Monad m) => (a -> m Ordering) -> a -> a -> m a
binSearchM f l h
  | h < l     = pure l
  | otherwise = f mid >>= \case
      GT -> binSearchM f l (mid-1)
      LT -> binSearchM f (mid+1) h
      _  -> pure mid
  where
    mid = l + (h-l) `div` 2

-- | Find a local minimum.
findMin :: Ord b => (a -> b) -> [a] -> a
findMin _ [] = error "findMin: no inputs"
findMin f (x:xs) = go xs x
  where go [] r = r
        go (x':xs') r
          | f x' > f r = r
          | otherwise = go xs' x'

-- | Find a local maximum.
findMax :: Ord b => (a -> b) -> [a] -> a
findMax _ [] = error "findMax: no inputs"
findMax f (x:xs) = go xs x
  where go [] r = r
        go (x':xs') r
          | f x' < f r = r
          | otherwise = go xs' x'

-- | Count the number of items for which this is true.
countIf :: (a -> Bool) -> [a] -> Int
countIf f = length . filter f

-- | Make variations of a list by changing an element.
perturb :: (a -> [a]) -> [a] -> [[a]]
perturb f = go
  where
    go []     = []
    go (x:xs) = ((:xs) <$> f x) <> ((x:) <$> go xs)

-- Find arrangements of a such that our predicate is happy.
arranger :: (a -> b -> Bool) -> [a] -> [b] -> Maybe [a]
arranger p as bs = go (select as) bs []
  where
    go [] [] r = Just (reverse r)
    go ((a,as'):more) allb@(b:bs') r
      | p a b = go (select as') bs' (a:r) <|> go more allb r
      | otherwise = go more allb r
    go _ _ _ = Nothing
