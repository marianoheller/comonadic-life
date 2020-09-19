import Data.Proxy
import Lib
import Spec.Comonad (testComonadLaws)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck.Laws.Functor (testFunctorLaws)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ lawsZL, unitTests]

lawsZL :: TestTree
lawsZL = testGroup "laws ZL" [functorLZLaws, comonadLZLaws]

functorLZLaws :: TestTree
functorLZLaws =
  testFunctorLaws
    (Proxy :: Proxy LZ)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (\_ -> (==))

comonadLZLaws :: TestTree
comonadLZLaws =
  testComonadLaws
    (Proxy :: Proxy LZ)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (Proxy :: Proxy Bool)
    (\_ -> (==))
    (\_ -> (==))
  where
    debug eq a b = d `seq` eq a b
      where
        d = unsafePerformIO $ putStrLn $ show a ++ " | " ++ show b

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "iterate until sample" $
        interateUntil ((+) 1) ((==) 5) 1 @?= [2, 3, 4, 5]
    ]