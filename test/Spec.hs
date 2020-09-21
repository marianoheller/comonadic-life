import Control.Comonad
import Data.Proxy
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck.Laws.Comonad (testComonadLaws)
import Test.Tasty.QuickCheck.Laws.Functor (testFunctorLaws)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lawsZL, lawsZ, unitTests]

lawsZL :: TestTree
lawsZL = testGroup "laws ZL" [functorLZLaws, comonadLZLaws]

lawsZ :: TestTree
lawsZ = testGroup "laws Z" [functorZLaws, comonadZLaws]

functorLZLaws :: TestTree
functorLZLaws =
  testFunctorLaws
    (Proxy :: Proxy LZ)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (const (==))

functorZLaws :: TestTree
functorZLaws =
  testFunctorLaws
    (Proxy :: Proxy Z)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (const (==))

comonadLZLaws :: TestTree
comonadLZLaws =
  testComonadLaws
    (Proxy :: Proxy LZ)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (const (==))
    (const (==))

comonadZLaws :: TestTree
comonadZLaws =
  testComonadLaws
    (Proxy :: Proxy Z)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (const (==))
    (const (==))

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ iterateUntilTests,
      aliveNeighboursTests
    ]

iterateUntilTests :: TestTree
iterateUntilTests =
  testGroup
    "interate until unit tests"
    [ testCase "iterate until sample" $
        interateUntil ((1 :: Int) +) (5 ==) 1 @?= [2, 3, 4, 5],
      testCase "iterate until sample string" $
        interateUntil ("a" ++) ("aaa" ==) "a" @?= ["aa", "aaa"]
    ]

aliveNeighboursTests :: TestTree
aliveNeighboursTests =
  testGroup
    "neighbours unit tests"
    [ testCase "nieghbours with no alive" $
        let grid = Z $ duplicate $ LZ [False] False [False]
         in aliveNeighbours grid @?= 0,
      testCase "nieghbours with 2 alive" $
        let grid = Z $ duplicate $ LZ [False] True [False]
         in aliveNeighbours grid @?= 2,
      testCase "nieghbours with 6 alive" $
        let grid = Z $ duplicate $ LZ [True] False [True]
         in aliveNeighbours grid @?= 6,
      testCase "nieghbours with 8 alive" $
        let grid = Z $ duplicate $ LZ [True] True [True]
         in aliveNeighbours grid @?= 8
    ]
