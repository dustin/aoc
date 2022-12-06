{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : BitSet
Description : A Set-type structure that stores values as bits.
Copyright   : (c) Dustin Sallings, 2019
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

A Set-type structure that stores values as bits in a bitmap.
-}
module Advent.BitSet where

import           Control.DeepSeq (NFData (..))
import           Data.Bits       (Bits (..), bitSizeMaybe, clearBit, popCount, setBit, testBit, (.|.))
import           Data.Ix         (Ix (..))
import           Data.List       (foldl', intercalate)
import           Data.Maybe      (fromMaybe)

data BitSet i w = BitSet !(i,i) !w

instance Eq w => Eq (BitSet i w) where
  {-# INLINE (==) #-}
  (BitSet _ a) == (BitSet _ b) = a == b

instance Ord w => Ord (BitSet i w) where
  {-# INLINE compare #-}
  compare (BitSet _ a) (BitSet _ b) = compare a b

instance NFData (BitSet i w) where
  rnf (BitSet _ a) = a `seq` ()

bitSet :: (Bits w, Ix i) => (i,i) -> BitSet i w
bitSet ins
  | rangeSize ins > fromMaybe maxBound (bitSizeMaybe z) = error "bitset range exceeds storage capacity"
  | otherwise = BitSet ins z
  where
    z = zeroBits

{-# INLINE insert #-}
insert :: (Bits w, Ix i) => i -> BitSet i w -> BitSet i w
insert i (BitSet r x) = BitSet r $ x `setBit` index r i

{-# INLINE null #-}
null :: Bits w => BitSet i w -> Bool
null (BitSet _ w) = w == zeroBits

{-# INLINE length #-}
length :: Bits w => BitSet i w -> Int
length (BitSet _ x) = popCount x

{-# INLINE member #-}
member :: (Bits w, Ix i) => i -> BitSet i w -> Bool
member i (BitSet r x) = testBit x (index r i)

{-# INLINE notMember #-}
notMember :: (Bits w, Ix i) => i -> BitSet i w -> Bool
notMember i = not . member i

{-# INLINE delete #-}
delete :: (Bits w, Ix i) => i -> BitSet i w -> BitSet i w
delete i (BitSet r x) = BitSet r $ x `clearBit` index r i

{-# INLINE isSubsetOf #-}
isSubsetOf :: Bits w => BitSet i w -> BitSet i w -> Bool
isSubsetOf (BitSet _ a) (BitSet _ b) = b == b .|. a

{-# INLINE union #-}
union :: Bits w => BitSet i w -> BitSet i w -> BitSet i w
union (BitSet i a) (BitSet _ b) = BitSet i (a .|. b)

{-# INLINE intersection #-}
intersection :: Bits w => BitSet i w -> BitSet i w -> BitSet i w
intersection (BitSet i a) (BitSet _ b) = BitSet i (a .&. b)

{-# INLINE difference #-}
difference :: Bits w => BitSet i w -> BitSet i w -> BitSet i w
difference (BitSet i a) (BitSet _ b) = BitSet i (a .&. complement b)

{-# INLINE disjoint #-}
disjoint :: Bits w => BitSet i w -> BitSet i w -> Bool
disjoint (BitSet _ a) (BitSet _ b) = a .&. b == zeroBits

toList :: (Bits w, Ix i) => BitSet i w -> [i]
toList bs@(BitSet r _) = Prelude.filter (`member` bs) $ range r

instance Bits w => Semigroup (BitSet i w) where
  {-# INLINE (<>) #-}
  (<>) = union

instance (Bits w, Show i, Ix i) => Show (BitSet i w) where
  show bs@(BitSet r _) = "fromList " <> show r <> " [" <> intercalate ", " (map show (toList bs)) <> "]"

fromList :: (Bits w, Ix i) => (i,i) -> [i] -> BitSet i w
fromList r = BitSet r . foldl' (\o x -> o `setBit` index r x) zeroBits

filter :: (Bits w, Ix i) => (i -> Bool) -> BitSet i w -> BitSet i w
filter f bs@(BitSet i _) = fromList i . Prelude.filter f . toList $ bs

findMin :: (Bits w, Ix i) => BitSet i w -> i
findMin = head . toList

alterF :: (Bits w, Ix i, Functor f) => (Bool -> f Bool) -> i -> BitSet i w -> f (BitSet i w)
alterF f k s = fmap alter (f (member k s))
  where
    alter True = insert k s
    alter False = delete k s
