{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Control.Comonad
import Control.Monad (liftM2, replicateM)
import GHC.Generics (Generic)
import System.Random (Random (randomIO))
import Test.Tasty.QuickCheck

data LZ a = LZ [a] a [a] deriving (Eq, Show, Generic)

leftLZ :: LZ a -> LZ a
leftLZ (LZ [] a r) = LZ [] a r
leftLZ (LZ (l : ls) a r) = LZ ls l (a : r)

rightLZ :: LZ a -> LZ a
rightLZ (LZ l a []) = LZ l a []
rightLZ (LZ l a (r : rs)) = LZ (a : l) r rs

isEndLZ :: LZ a -> Bool
isEndLZ (LZ _ _ []) = True
isEndLZ _ = False

isStartLZ :: LZ a -> Bool
isStartLZ (LZ [] _ _) = True
isStartLZ _ = False

write :: a -> LZ a -> LZ a
write a (LZ l _ r) = LZ l a r

read :: LZ a -> a
read (LZ _ a _) = a

toListN :: LZ a -> Int -> [a]
toListN (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor LZ where
  fmap f (LZ l a r) = LZ (fmap f l) (f a) (fmap f r)

instance Comonad LZ where
  extract (LZ _ a _) = a
  duplicate z = LZ (moveLeft z) z (moveRight z)
    where
      moveLeft = interateUntil leftLZ isStartLZ
      moveRight = interateUntil rightLZ isEndLZ

interateUntil :: (a -> a) -> (a -> Bool) -> a -> [a]
interateUntil f predicate seed =
  case (predicate next, predicate seed) of
    (True, True) -> []
    (True, _) -> [next]
    _ -> next : interateUntil f predicate next
  where
    next = f seed

instance Arbitrary a => Arbitrary (LZ a) where
  arbitrary = do
    a <- arbitrary
    ls <- vector 10 :: Arbitrary a => Gen [a]
    rs <- vector 10 :: Arbitrary a => Gen [a]
    return $ LZ ls a rs

instance CoArbitrary a => CoArbitrary (LZ a) where
  coarbitrary = genericCoarbitrary

{- -------------------------------------------------- -}
{- Z -}
newtype Z a = Z (LZ (LZ a)) deriving (Eq, Show, Generic)

upZ :: Z a -> Z a
upZ (Z lz) = Z (leftLZ lz)

downZ :: Z a -> Z a
downZ (Z lz) = Z (rightLZ lz)

leftZ :: Z a -> Z a
leftZ (Z lz) = Z (fmap leftLZ lz)

rightZ :: Z a -> Z a
rightZ (Z lz) = Z (fmap rightLZ lz)

isTopZ :: Z a -> Bool
isTopZ (Z lz) = isStartLZ lz

isBottomZ :: Z a -> Bool
isBottomZ (Z lz) = isEndLZ lz

isLeftZ :: Z a -> Bool
isLeftZ (Z (LZ _ lz _ )) = isStartLZ lz

isRightZ :: Z a -> Bool
isRightZ (Z (LZ _ lz _ )) = isEndLZ lz

instance Functor Z where
  fmap f (Z a) = Z $ (fmap . fmap) f a

instance Comonad Z where
  extract (Z a) = extract $ extract a
  duplicate z = Z $ horizontal <$> vertical z
    where
      vertical zipper = LZ (interateUntil upZ isTopZ zipper) zipper (interateUntil downZ isBottomZ zipper)
      horizontal zipper = LZ (interateUntil leftZ isLeftZ zipper) zipper (interateUntil rightZ isRightZ zipper)

instance Arbitrary a => Arbitrary (Z a) where
  arbitrary = do
    lz <- arbitrary :: Arbitrary a => Gen (LZ a)
    return $ Z $ duplicate lz

instance CoArbitrary a => CoArbitrary (Z a) where
  coarbitrary = genericCoarbitrary


{- -------------------------------------------------- -}
{- Game Logic -}

neighbours :: [Z a -> Z a]
neighbours =
  horiz ++ vert ++ liftM2 (.) horiz vert
  where
    horiz = [leftZ, rightZ]
    vert = [upZ, downZ]

aliveNeighbours :: Z Bool -> Int
aliveNeighbours z =
  card $ map (\dir -> extract $ dir z) neighbours

card :: [Bool] -> Int
card = length . filter (== True)

rule :: Z Bool -> Bool
rule z =
  case aliveNeighbours z of
    2 -> extract z
    3 -> True
    _ -> False

evolve :: Z Bool -> Z Bool
evolve = extend rule

{- -------------------------------------------------- -}
{- Game Display -}

dispLine :: LZ Bool -> String
dispLine z =
  map dispC $ toListN z 6
  where
    dispC True = '*'
    dispC False = ' '

disp :: Z Bool -> String
disp (Z z) =
  unlines $ map dispLine $ toListN z 6

{- -------------------------------------------------- -}
{- Gen -}

makeRow :: Int -> IO [Bool]
makeRow n = replicateM n randomIO

makeGrid :: Int -> IO [[Bool]]
makeGrid n = replicateM n (makeRow n)
