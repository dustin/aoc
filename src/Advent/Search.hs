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
{-# LANGUAGE ViewPatterns #-}

module Advent.Search (
  -- * Graph searching
  dijkstra', dijkstra, resolveAStar,
  bfs, bfsOn, bfsOnInt, astar, astar',
  dfs, dfsOn, dfsWith,
  astarOn, astarOn',
  multiAstarOn, multiAstarOn',
  bfsM, bfsOnM,
  flood,
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
import qualified Data.IntSet                 as IntSet
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (listToMaybe)
import qualified Data.PQueue.Min             as Q
import qualified Data.Set                    as Set
import Data.Set (Set)

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
    loop seen inq =
      case Queue.pop inq of
        Nothing -> []
        Just (x,xs)
          | Set.member r seen ->     loop seen xs
          | otherwise            -> x : loop (Set.insert r seen) (Queue.appendList xs $ next x)
          where r = rep x

-- | bfsOnInt finds all reachable states from a given value and a
-- function to find its neighbors using a representative function.
--
-- This is a specialization of bfsOn where the representation is an Int.
bfsOnInt ::
  (a -> Int)   {- ^ representative function -} ->
  (a -> [a]) {- ^ neighbors               -} ->
  a          {- ^ initial state           -} ->
  [a]        {- ^ reachable states        -}
bfsOnInt rep next start = loop IntSet.empty (Queue.singleton start)
  where
    loop seen inq =
      case Queue.pop inq of
        Nothing -> []
        Just (x,xs)
          | IntSet.member r seen ->     loop seen xs
          | otherwise            -> x : loop (IntSet.insert r seen) (Queue.appendList xs $ next x)
          where r = rep x

-- | bfsM finds all reachable states from a given value and a function
-- to find its neighbors.
bfsM :: (Monad m, Ord a)
     => (a -> m [a]) -- ^ neighbors
     -> a            -- ^ initial state
     -> m [a]        -- ^ reachable states
bfsM = bfsOnM id

-- | bfsOn finds all reachable states from a given value and a
-- function to find its neighbors using a representative function.
--
-- The representative function allows a separation between the
-- structure of a state and how it's represented, removing the Ord
-- requirement for the state.
bfsOnM :: (Monad m, Ord r)
       => (a -> r)     -- ^ representative function
       -> (a -> m [a]) -- ^ neighbors
       -> a            -- ^ initial state
       -> m [a]        -- ^ reachable states
bfsOnM rep next start = loop Set.empty (Queue.singleton start)
  where
    loop seen inq = case Queue.pop inq of
      Nothing -> pure []
      Just (x, xs)
        | Set.member r seen -> loop seen xs
        | otherwise         -> next x >>= \n -> (x:) <$> loop (Set.insert r seen) (Queue.appendList xs n)
        where
          r = rep x

-- | DFS.
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id

-- | A DFS variant of 'bfsOn'.
dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn rep = dfsWith (Set.insert . rep) (Set.member . rep)

-- | BFS with custom functions for remembering and recalling whether a state has been visited.
dfsWith :: Monoid s => (a -> s -> s) -> (a -> s -> Bool) -> (a -> [a]) -> a -> [a]
dfsWith remember seenf next start = go mempty [start]
  where
    go _ [] = []
    go seen (x:xs)
        | seenf x seen = go seen xs
        | otherwise    = x : go (remember x seen) (next x <> xs)

-- Tests for this are in 2018 Day 22.

-- | 'dijkstra'' uses [Dijkstra's
-- Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to
-- find the costs and links from a starting point to various points on
-- a graph.
--
-- See 'resolveAStar' for a means of determining a path from the
-- resulting values.
dijkstra' :: Ord v => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
  -> v -- ^ The starting point.
  -> (v -> Bool) -- ^ Predicate allowing early termination of search (if True).
  -> Maybe (v, Map v (Int, v)) -- ^ (cost from origin to v, links from points to points)
dijkstra' nf = astar' nf (const 0)

-- | Using maps computed by 'astar'', find the cost and path from the
-- start to a destination.
resolveAStar :: Ord r => Map r (Int, r) -> r -> r -> Maybe (Int,[r])
resolveAStar m start end = case Map.lookup end m of
                             Nothing       -> Nothing
                             Just (cost,_) -> Just (cost, reverse $ end : go end)
  where
    go pt
      | pt == start = []
      | otherwise = next : go next
      where next = snd (m Map.! pt)

-- | 'dijkstra' uses [Dijkstra's
-- Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to
-- find the lowest cost path to a given destination.
dijkstra :: Ord v => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
         -> v -- ^ The starting point.
         -> (v -> Bool) -- ^ Goal function
         -> Maybe (Int,[v])  -- ^ The cost to the destination, and the path to get there.
dijkstra neighbrf start goal = resolve =<< dijkstra' neighbrf start goal
  where resolve (e, m) = resolveAStar m start e

-- | A* search.
astar' :: Ord v
  => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
  -> (v -> Int) -- ^ Heuristic
  -> v -- ^ The starting point.
  -> (v -> Bool) -- ^ Predicate allowing early termination of search (if True).
  -> Maybe (v, Map v (Int, v)) -- ^ (cost from origin to v, links from points to points)
astar' = astarOn' id

newtype Unordered a = Unordered a

instance Eq (Unordered a) where _ == _ = False

instance Ord (Unordered a) where compare _ _ = LT

-- | A* search.
astarOn' :: Ord r
  => (v -> r) -- ^ Representation function
  -> (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
  -> (v -> Int) -- ^ Heuristic
  -> v -- ^ The starting point.
  -> (v -> Bool) -- ^ Predicate allowing early termination of search (if True).
  -> Maybe (v, Map r (Int, r)) -- ^ (cost from origin to v, links from points to points)
astarOn' rf nf hf start = multiAstarOn' rf nf hf [start]

-- | A* search.
astar :: Ord v
      => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
      -> (v -> Int) -- ^ Heuristic
      -> v -- ^ The starting point.
      -> (v -> Bool) -- ^ Goal function
      -> Maybe (Int,[v])  -- ^ The cost to the destination, and the path to get there.
astar = astarOn id

-- | A* search.
astarOn :: Ord r
        => (v -> r) -- ^ Representation function.
        -> (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
        -> (v -> Int) -- ^ Heuristic
        -> v -- ^ The starting point.
        -> (v -> Bool) -- ^ Goal function
        -> Maybe (Int,[r])  -- ^ The cost to the destination, and the path to get there.
astarOn rf nf hf start goal = resolve =<< astarOn' rf nf hf start goal
  where resolve (e, m) = resolveAStar m (rf start) (rf e)

-- | A* search with multiple starting points.
multiAstarOn :: Ord r
              => (v -> r)
              -> (v -> [(Int,v)])
              -> (v -> Int)
              -> [v]
              -> (v -> Bool)
              -> Maybe (Int,[r])
multiAstarOn rf nf hf starts goal = resolve =<< multiAstarOn' rf nf hf starts goal
  where
    resolve (e, m) = res' m (rf <$> starts) (rf e)
    res' m startsr end = case Map.lookup end m of
                             Nothing       -> Nothing
                             Just (cost,_) -> Just (cost, reverse $ end : go end)
      where
        go pt
          | pt `elem` startsr = []
          | otherwise = next : go next
          where next = snd (m Map.! pt)

-- | A* search with multiple starting points.
multiAstarOn' :: Ord r
              => (v -> r)
              -> (v -> [(Int,v)])
              -> (v -> Int)
              -> [v]
              -> (v -> Bool)
              -> Maybe (v, Map r (Int, r))
multiAstarOn' rf nf hf starts done = go setup initmap mempty
  where
    setup = Q.fromList [(0, hf x, Unordered x) | x <- starts]
    initmap = Map.fromList [(rf x, (0, rf x)) | x <- starts]
    go q m seen
      | Q.null q = Nothing
      | done pt = Just (pt, m')
      | Set.member (rf pt) seen = go odo m seen
      | otherwise = go (odo <> psd) m' (Set.insert (rf pt) seen)

      where
        ([(d,_, Unordered pt)], odo) = Q.splitAt 1 q
        moves = filter (\(c,p') -> c+d < maybe (c+d+1) fst (Map.lookup (rf p') m)) (nf pt) `using` parList rseq
        m' = Map.union (Map.fromList [(rf p', (c+d, rf pt)) | (c,p') <- moves]) m
        psd = Q.fromList [(d+c, hf x, Unordered x) | (c,x) <- moves]

-- | Flood fill a graph and return the set of all reachable nodes.
flood :: Ord a => (a -> Set a) -> a -> Set a
flood nf = go mempty . Set.singleton
    where
        go s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map nf todo)

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

-- | Find an arrangement of @a@ such that our predicate is happy for each element in @b@.
--
-- e.g.
--
-- >>> arranger (<) [1,3,2,4] [3,6,2,6]
-- Just [2, 3, 1, 4]
arranger :: (a -> b -> Bool) -> [a] -> [b] -> Maybe [a]
arranger p as bs = go (select as) bs []
  where
    go [] [] r = Just (reverse r)
    go ((a,as'):more) allb@(b:bs') r
      | p a b = go (select as') bs' (a:r) <|> go more allb r
      | otherwise = go more allb r
    go _ _ _ = Nothing
