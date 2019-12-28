module VisTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit      hiding (assert)
import           Test.Tasty.QuickCheck as QC

import           Advent.Vis

prop_listBounds :: NonEmptyList Int -> NonEmptyList Int -> Property
prop_listBounds (NonEmpty xsin) (NonEmpty ysin) =
  listBounds z === ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    z = zip xsin ysin
    xs = fst <$> z
    ys = snd <$> z

tests :: [TestTree]
tests = [
  testProperty "listBounds" prop_listBounds
  ]
