{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Advent.Queue where

import           Data.Foldable (Foldable (..))

data Queue a = Queue [a] [a]

instance Show a => Show (Queue a) where
  show q = "Queue.fromList [" <> show (toList q) <> "]"

instance Semigroup (Queue a) where
  (Queue l1 r1) <> (Queue l2 r2) = Queue l1 (r1 <> l2 <> reverse r2)

instance Monoid (Queue a) where
  mempty = Queue [] []

instance Foldable Queue where
  toList (Queue l r) = l <> reverse r
  foldr f i (Queue l r) = foldr f (foldl (flip f) i r) l
  null (Queue l r) = null l && null r

fromList :: [a] -> Queue a
fromList l = Queue l []

snoc :: a -> Queue a -> Queue a
snoc a (Queue [] r) =  Queue (reverse (a:r)) []
snoc a (Queue l r)  = Queue l (a:r)

head :: Queue a -> a
head (Queue (l:_) _) = l
head _               = error "headless queue"

appendList :: Queue a -> [a] -> Queue  a
appendList (Queue [] []) xs = Queue xs []
appendList (Queue [] r) xs  = Queue (reverse r) (reverse xs)
appendList (Queue l r) xs   = Queue l (reverse xs <> r)

{-# COMPLETE (:<|), Empty #-}

pattern Empty :: Queue a
pattern Empty <- Queue [] _
  where
    Empty = Queue [] []

pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x, xs))

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:xs) r) = Just (x, if null xs then Queue (reverse r) [] else Queue xs r)
pop _                = Nothing

singleton :: a -> Queue a
singleton a = Queue [a] []
