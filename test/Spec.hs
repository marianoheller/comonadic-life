import Data.Proxy
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck.Laws.Functor (testFunctorLaws)
import Test.Tasty.QuickCheck.Laws.Comonad (testComonadLaws)
import Control.Comonad

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lawsZL, lawsZ, unitTests, unitTestsZ]

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
    [ testCase "iterate until sample" $
        interateUntil ((1 :: Int) +) (5 ==) 1 @?= [2, 3, 4, 5]
    , testCase "iterate until sample string" $
        interateUntil ("a" ++) ("aaa" ==) "a" @?= ["aa", "aaa"]
    ]

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

unitTestsZ :: TestTree
unitTestsZ =
  let
    f = extract :: (Z a -> a)
    g = extract :: (Z a -> a)
    x = extend f . extend g
    y = extend (f . extend g)
    samples = [ LZ [] True []
              , LZ [True] False [True]
              , LZ [True, True, True] False []
              , LZ [True, True] False [True]
              , LZ [True, True] False [True, True]
              ]
  in
  testGroup
    "Unit tests" $
    mapInd (\sample -> \ind ->
            testCase ("sample " ++ show ind) $
              let
                z = Z $ duplicate sample
              in
                x z @?= y z
            ) samples
