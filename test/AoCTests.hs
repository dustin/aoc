module AoCTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)

import qualified Data.Map.Strict  as Map

import           Advent.AoC

testParseGrid :: Assertion
testParseGrid = assertEqual "" (Map.fromList [((0,0), 'a'), ((1,0), 'b'),
                                              ((0,1), 'c'), ((1,1), 'd')]) $
                parseGrid id "ab\ncd\n"

tests :: [TestTree]
tests = [
  testCase "parse grid test" testParseGrid
  ]
