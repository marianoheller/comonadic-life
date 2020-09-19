import Lib
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck.Laws.Comonad

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $
        \list -> (list :: [Int]) == list
    ]
{- 
comonadZLaws :: TestTree
comonadZLaws =
  testComonadLaws
    Zipper
    undefined
    undefined
    undefined
    undefined
    undefined
    undefined -}

unitTests =
  testGroup
    "Unit tests"
    []