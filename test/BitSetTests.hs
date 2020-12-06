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

propFromToList :: SomeLowers -> Property
propFromToList (SomeLowers ls) = Set.fromList ls =.= fromChars ls

propInsert :: SomeLowers -> Property
propInsert (SomeLowers ls) = foldr Set.insert mempty ls =.= foldr BitSet.insert emptyBS ls

propNull :: SomeLowers -> Property
propNull (SomeLowers ls) = Set.null (Set.fromList ls) === BitSet.null (fromChars ls)

propLength :: SomeLowers -> Property
propLength (SomeLowers ls) = Set.size (Set.fromList ls) === BitSet.length (fromChars ls)

comp :: (Show a, Eq a) => (Char -> Set Char -> a) -> (Char -> CharSet -> a) -> SomeLowers -> SomeLowers -> Property
comp sf bf (SomeLowers ls) (SomeLowers ts) = ((`sf` s) <$> ts) === ((`bf` bs) <$> ts)
  where
    s = Set.fromList ls
    bs = fromChars ls

propMember :: SomeLowers -> SomeLowers -> Property
propMember = comp Set.member BitSet.member

propNotMember :: SomeLowers -> SomeLowers -> Property
propNotMember =  comp Set.notMember BitSet.notMember

propDelete :: SomeLowers -> SomeLowers -> Property
propDelete = comp (\c -> Set.toList . Set.delete c) (\c -> BitSet.toList . BitSet.delete c)

propSubset :: SomeLowers -> SomeLowers -> Property
propSubset (SomeLowers a) (SomeLowers b) = (Set.fromList a `Set.isSubsetOf` Set.fromList b)
                                           === (fromChars a `BitSet.isSubsetOf` fromChars b)

propDisjoint :: SomeLowers -> SomeLowers -> Property
propDisjoint (SomeLowers a) (SomeLowers b) = (Set.fromList a `Set.disjoint` Set.fromList b)
                                             === (fromChars a `BitSet.disjoint` fromChars b)

comp2 :: (Set Char -> Set Char -> Set Char) -> (CharSet -> CharSet -> CharSet) -> SomeLowers -> SomeLowers -> Property
comp2 sf bf (SomeLowers a) (SomeLowers b) = (Set.fromList a `sf` Set.fromList b) =.= (fromChars a `bf` fromChars b)

propUnion :: SomeLowers -> SomeLowers -> Property
propUnion = comp2 Set.union BitSet.union

propIntersection :: SomeLowers -> SomeLowers -> Property
propIntersection = comp2 Set.intersection BitSet.intersection

propFilter :: SomeLowers -> Property
propFilter (SomeLowers a) = Set.filter isVowel (Set.fromList a) =.= BitSet.filter isVowel (fromChars a)
  where isVowel = (`elem` ['a', 'e', 'i', 'o', 'u'])

propFindMin :: SomeLowers -> Property
propFindMin (SomeLowers a) = (not.null) a ==> (Set.findMin . Set.fromList) a === (BitSet.findMin . fromChars) a

tests :: [TestTree]
tests = [
  testProperty "fromList" propFromToList,
  testProperty "insert" propInsert,
  testProperty "null" propNull,
  testProperty "length" propLength,
  testProperty "member" propMember,
  testProperty "notMember" propNotMember,
  testProperty "delete" propDelete,
  testProperty "isSubsetOf" propSubset,
  testProperty "disjoint" propDisjoint,
  testProperty "union" propUnion,
  testProperty "intersection" propIntersection,
  testProperty "filter" propFilter,
  testProperty "findMin" propFindMin
  ]
