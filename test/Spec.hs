import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified AoCTests
import qualified BitSetTests
import qualified SearchTests
import qualified VisTests

tests :: [TestTree]
tests = [
  testGroup "aoc" AoCTests.tests,
  testGroup "search" SearchTests.tests,
  testGroup "bitset" BitSetTests.tests,
  testGroup "vis" VisTests.tests
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
