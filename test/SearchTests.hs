module SearchTests where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit        hiding (assert)
import           Test.Tasty.QuickCheck   as QC

import           Data.List               (elemIndex, sort)

import           Advent.Search

cycling :: Int -> Int -> [Int]
cycling a b = let (h,xs) = splitAt a [0..] in h <> cycle (take b xs)

prop_findCycle :: NonNegative (Small Int) -> Positive (Small Int) -> Bool
prop_findCycle (NonNegative (Small a)) (Positive (Small b)) = findCycle id cl == (a,b,cl !! a)
  where cl = cycling a b

-- Ended up not using this.
cyclen :: Eq a => [a] -> Maybe Int
cyclen []     = Nothing
cyclen [_]    = Nothing
cyclen (x:xs) = (1+) <$> elemIndex x xs

prop_cyclen :: NonNegative (Small Int) -> NonNegative (Small Int) -> Positive (Small Int) -> Bool
prop_cyclen (NonNegative (Small a)) (NonNegative (Small b)) (Positive (Small c)) =
  cyclen (drop (a + b) (cycling a c)) == Just c

prop_binSearch :: Int -> Int -> Int -> Bool
prop_binSearch a b c = let [a',b',c'] = sort [a,b,c] in
                         binSearch (`compare` b') a' c' == b'

prop_BinSearchM :: Int -> Int -> Int -> Property
prop_BinSearchM a b c = monadicIO $ do
  let [a',b',c'] = sort [a,b,c]
  r <- binSearchM (\x -> compare x <$> (run . getB) b') a' c'
  assert $ r == b'

  where getB :: Int -> IO Int
        getB = pure


-- autoBinSearch finds a value without having to be told bounds, so just needs one number.
prop_AutoBinSearch :: Int -> Bool
prop_AutoBinSearch a = autoBinSearch (`compare` a) == a


test_FindMin :: [TestTree]
test_FindMin = map (\(f, xs, want) -> testCase (show xs) $ assertEqual "" want (findMin f xs)) [
  (id, [9], 9),
  (id, [9, 7, 5, 4, 3, 2, 3, 1], 2)
  ]

-- Every value before the min value should be greater than the min value.
propMin :: NonEmptyList Int -> Bool
propMin (NonEmpty xs) = let n = findMin id xs in
                          and . (\l -> zipWith (>=) l $ tail l) . takeWhile (/= n) $ xs

-- Every value before the min value should be less than the max value.
propMax :: NonEmptyList Int -> Bool
propMax (NonEmpty xs) = let n = findMax id xs in
                          and . (\l -> zipWith (<=) l $ tail l) . takeWhile (/= n) $ xs


-- Verify findMin ≠ minimum by ensuring there's a small uptick and
-- then still yet lower value at the end of the list.
prop_findMinNotMin :: NonEmptyList Int -> Bool
prop_findMinNotMin (NonEmpty xs) = let mn = minimum xs
                                       l = xs <> [succ mn, (pred.pred) mn] in
                                     findMin id l /= minimum l

unit_perturb :: Assertion
unit_perturb = do
  let p = perturb (\x -> case even x of True -> [x * 2, negate x]; _ -> []) [1..5]
  assertEqual "" [[1,4,3,4,5],[1,-2,3,4,5],[1,2,3,8,5],[1,2,3,-4,5]] p

prop_arranger :: [Int] -> Property
prop_arranger alist = arranger (==) alist (sort alist) === Just (sort alist)

test_search :: [TestTree]
test_search = [
  localOption (QC.QuickCheckTests 10000) $ testProperty "findMin" propMin,
  localOption (QC.QuickCheckTests 10000) $ testProperty "findMax" propMax
  ]
