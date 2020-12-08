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
import           Data.Bits       (Bits (..), clearBit, popCount, setBit, testBit, (.|.))
import           Data.Ix         (Ix (..))
import           Data.List       (intercalate)

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
  | rangeSize ins > 32 = error "e too big"
  | otherwise = BitSet ins zeroBits

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
fromList r = foldr insert (bitSet r)

filter :: (Bits w, Ix i) => (i -> Bool) -> BitSet i w -> BitSet i w
filter f bs@(BitSet i _) = fromList i . Prelude.filter f . toList $ bs

findMin :: (Bits w, Ix i) => BitSet i w -> i
findMin = head . toList
