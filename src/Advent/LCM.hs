{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Advent.LCM
Description : An LCM Monoid.
Copyright   : (c) Dustin Sallings, 2023
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Rather than doing some kind of fold1 or foldr 1 or whatever, just have an LCM monoid and do a regular fold.
-}

module Advent.LCM where

import           Data.Coerce
import Data.Data ( Data, Typeable )
import           Data.Foldable (Foldable (foldl', foldr', toList))
import           GHC.Generics

-- | LCM Integral monoid.
newtype (LCM a) = LCM { getLCM :: a }
  deriving stock (Show, Eq, Ord, Read, Bounded, Data, Typeable, Generic)
  deriving newtype (Num)

instance (Integral a) => Semigroup (LCM a) where
  LCM a <> LCM b = LCM (lcm a b)

instance (Integral a) => Monoid (LCM a) where
  mempty = LCM 1

instance Functor LCM where
    fmap     = coerce

instance Applicative LCM where
    pure     = LCM
    (<*>)    = coerce

instance Monad LCM where
    m >>= k  = k (getLCM m)

-- Stolen from Data.Functor.Utils
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

instance Foldable LCM where
    foldMap            = coerce

    elem               = (. getLCM) #. (==)
    foldl              = coerce
    foldl'             = coerce
    foldl1 _           = getLCM
    foldr f z (LCM x)  = f x z
    foldr'             = foldr
    foldr1 _           = getLCM
    length _           = 1
    maximum            = getLCM
    minimum            = getLCM
    null _             = False
    product            = getLCM
    sum                = getLCM
    toList (LCM  x)     = [x]


instance Traversable LCM where
    traverse f (LCM x) = LCM <$> f x
