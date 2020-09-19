import Data.Proxy
import Lib
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck.Laws.Comonad (testComonadLaws)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [comonadLZLaws, unitTests]

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

unitTests =
  testGroup
    "Unit tests"
    []