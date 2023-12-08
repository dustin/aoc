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

-- | LCM Integral monoid.
newtype (LCM a) = LCM { getLCM :: a }
  deriving stock (Show, Eq)
  deriving newtype (Num)

instance (Integral a) => Semigroup (LCM a) where
  LCM a <> LCM b = LCM (lcm a b)

instance (Integral a) => Monoid (LCM a) where
  mempty = LCM 1
