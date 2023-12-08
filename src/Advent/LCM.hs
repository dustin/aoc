{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Advent.LCM where

-- | LCM Integral monoid.
newtype (LCM a) = LCM { getLCM :: a }
  deriving stock (Show, Eq)
  deriving newtype (Num)

instance (Integral a) => Semigroup (LCM a) where
  LCM a <> LCM b = LCM (lcm a b)

instance (Integral a) => Monoid (LCM a) where
  mempty = LCM 1
