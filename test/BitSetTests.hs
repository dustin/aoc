module BitSetTests where

import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Word             (Word32)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit      hiding (assert)
import           Test.Tasty.QuickCheck as QC

import           Advent.BitSet         (BitSet)
import qualified Advent.BitSet         as BitSet

newtype SomeLowers = SomeLowers [Char] deriving Show

instance Arbitrary SomeLowers where
  arbitrary = SomeLowers <$> (choose (0, 32) >>= \n -> vectorOf n (choose charRange))
  shrink (SomeLowers x) = SomeLowers <$> shrink x

type CharSet = BitSet Char Word32

charRange :: (Char, Char)
charRange = ('a', 'z')

(=.=) :: Set Char -> CharSet -> Property
s =.= b = Set.toList s === BitSet.toList b

fromChars :: [Char] -> CharSet
fromChars = BitSet.fromList charRange

emptyBS :: CharSet
emptyBS = BitSet.bitSet charRange

prop_fromToList :: SomeLowers -> Property
prop_fromToList (SomeLowers ls) = Set.fromList ls =.= fromChars ls

prop_insert :: SomeLowers -> Property
prop_insert (SomeLowers ls) = foldr Set.insert mempty ls =.= foldr BitSet.insert emptyBS ls

prop_null :: SomeLowers -> Property
prop_null (SomeLowers ls) = Set.null (Set.fromList ls) === BitSet.null (fromChars ls)

prop_length :: SomeLowers -> Property
prop_length (SomeLowers ls) = Set.size (Set.fromList ls) === BitSet.length (fromChars ls)

comp :: (Show a, Eq a) => (Char -> Set Char -> a) -> (Char -> CharSet -> a) -> SomeLowers -> SomeLowers -> Property
comp sf bf (SomeLowers ls) (SomeLowers ts) = ((`sf` s) <$> ts) === ((`bf` bs) <$> ts)
  where
    s = Set.fromList ls
    bs = fromChars ls

prop_member :: SomeLowers -> SomeLowers -> Property
prop_member = comp Set.member BitSet.member

prop_notMember :: SomeLowers -> SomeLowers -> Property
prop_notMember =  comp Set.notMember BitSet.notMember

prop_delete :: SomeLowers -> SomeLowers -> Property
prop_delete = comp (\c -> Set.toList . Set.delete c) (\c -> BitSet.toList . BitSet.delete c)

prop_subset :: SomeLowers -> SomeLowers -> Property
prop_subset (SomeLowers a) (SomeLowers b) = (Set.fromList a `Set.isSubsetOf` Set.fromList b)
                                           === (fromChars a `BitSet.isSubsetOf` fromChars b)

prop_disjoint :: SomeLowers -> SomeLowers -> Property
prop_disjoint (SomeLowers a) (SomeLowers b) = (Set.fromList a `Set.disjoint` Set.fromList b)
                                             === (fromChars a `BitSet.disjoint` fromChars b)

comp2 :: (Set Char -> Set Char -> Set Char) -> (CharSet -> CharSet -> CharSet) -> SomeLowers -> SomeLowers -> Property
comp2 sf bf (SomeLowers a) (SomeLowers b) = (Set.fromList a `sf` Set.fromList b) =.= (fromChars a `bf` fromChars b)

prop_union :: SomeLowers -> SomeLowers -> Property
prop_union = comp2 Set.union BitSet.union

prop_intersection :: SomeLowers -> SomeLowers -> Property
prop_intersection = comp2 Set.intersection BitSet.intersection

prop_difference :: SomeLowers -> SomeLowers -> Property
prop_difference = comp2 Set.difference BitSet.difference

prop_filter :: SomeLowers -> Property
prop_filter (SomeLowers a) = Set.filter isVowel (Set.fromList a) =.= BitSet.filter isVowel (fromChars a)
  where isVowel = (`elem` ['a', 'e', 'i', 'o', 'u'])

prop_findMin :: SomeLowers -> Property
prop_findMin (SomeLowers a) = (not.null) a ==> (Set.findMin . Set.fromList) a === (BitSet.findMin . fromChars) a

unit_bigOne :: IO ()
unit_bigOne = do
  let big = BitSet.fromList (0, 8192) [0, 8000] :: BitSet Integer Integer
  assertEqual "" [0,8000] (BitSet.toList big)
