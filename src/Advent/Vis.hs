{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Vis
Description : Visualization helpers.
Copyright   : (c) Dustin Sallings, 2019
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Various things to help visualize data.
-}
module Advent.Vis where

import           Codec.Picture       (PixelRGB8 (..), generateImage, writePng)
import           Control.Exception   (bracket_)
import           Data.Coerce         (coerce)
import           Data.List           (intercalate)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Semigroup      (Max (..), Min (..))
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           System.Console.ANSI

white :: PixelRGB8
white = PixelRGB8 255 255 255

black :: PixelRGB8
black = PixelRGB8 0 0 0

red :: PixelRGB8
red = PixelRGB8 255 0 0

green :: PixelRGB8
green = PixelRGB8 0 255 0

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

someColors :: [PixelRGB8]
someColors = [red, green, blue]

class Bounded2D a where
  -- (min x, min y), (max x, max y)
  bounds2d :: a -> ((Int,Int), (Int,Int))

type PixelFun = (Int,Int) -> PixelRGB8

mapPixelFun :: Map (Int,Int) a -> (a -> PixelRGB8) -> PixelFun
mapPixelFun m f = f . (m Map.!)

mapPixelFunTrans :: Ord a => Map (Int, Int) a -> [(a, PixelRGB8)] -> PixelFun
mapPixelFunTrans m l = mapPixelFun m (\x -> Map.findWithDefault green x cm)
  where cm = Map.fromList l

listBounds :: (Foldable t, Integral a) => t (a,a) -> ((Int,Int),(Int,Int))
listBounds = coerce . foldMap f
  where
    f :: Integral a => (a,a) -> ((Min Int, Min Int), (Max Int, Max Int))
    f (x,y) = (((Min . fromIntegral) x, (Min . fromIntegral) y),
               ((Max . fromIntegral) x, (Max . fromIntegral) y))

instance Integral a => Bounded2D (Map (a, a) b) where
  bounds2d = listBounds . Map.keys

instance Integral a => Bounded2D (Set (a, a)) where
  bounds2d = listBounds . Set.toList

data DrawSpec = DrawSpec {
  width     :: Int,
  height    :: Int,
  transX    :: Int -> Int,
  transY    :: Int -> Int,
  invTransX :: Int -> Int,
  invTransY :: Int -> Int
  }

instance Show DrawSpec where
  show DrawSpec{..} = "DrawSpec{width=" <> show width <> ", height=" <> show height <> "}"

mkDrawSpec :: Bounded2D a => a -> DrawSpec
mkDrawSpec a = DrawSpec{..}
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d a
    width = mxx - mnx + 1
    height = mxy - mny + 1

    -- img x -> object x
    transX = (mnx +)
    transY = (mny +)

    invTransX = subtract mnx
    invTransY = subtract mny

draw :: Bounded2D a => FilePath -> a -> PixelFun -> IO ()
draw fn o pf = writePng fn (generateImage fromPF width height)
  where
    DrawSpec{..} = mkDrawSpec o
    fromPF x y = pf (transX x, transY y)

type CharFun = (Int, Int) -> Char

mapCharFun :: Map (Int,Int) a -> (a -> Char) -> CharFun
mapCharFun m f = f . (m Map.!)

mapCharFunTrans :: Ord a => Map (Int, Int) a -> [(a, Char)] -> CharFun
mapCharFunTrans m l = mapCharFun m (\x -> Map.findWithDefault '?' x cm)
  where cm = Map.fromList l

drawString :: Bounded2D a => a -> CharFun -> String
drawString a cf = intercalate "\n" (map (\y -> map (`fromPF` y) [0.. width - 1]) [0.. height - 1])
  where
    DrawSpec{..} = mkDrawSpec a
    fromPF x y = cf (transX x, transY y)

withHiddenCursor :: IO a -> IO a
withHiddenCursor =  bracket_ hideCursor showCursor

drawingBracket :: IO a -> IO a
drawingBracket = bracket_ begin end
  where begin = saveCursor
        end = setSGR [Reset] >> restoreCursor
