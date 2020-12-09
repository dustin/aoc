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

testLöb :: Assertion
testLöb = let fs = [ succ . (!! 1)
                   , succ . (!! 3)
                   , succ . (!! 0)
                   , const 1
                   ] in
            assertEqual "" [3, 2, 4, 1] (löb fs)

tests :: [TestTree]
tests = [
  testCase "parse grid test" testParseGrid,
  testCase "Löb's theorem" testLöb
  ]
