module Lib where

import Control.Comonad
import Control.Monad (liftM2)

data Zipper a = Zipper [a] a [a]

leftZipper :: Zipper a -> Zipper a
leftZipper (Zipper [] a r) = Zipper [] a r
leftZipper (Zipper (l : ls) a r) = Zipper ls l (a : r)

rightZipper :: Zipper a -> Zipper a
rightZipper (Zipper l a []) = Zipper l a []
rightZipper (Zipper l a (r : rs)) = Zipper (a : l) r rs

write :: a -> Zipper a -> Zipper a
write a (Zipper l _ r) = Zipper l a r

read :: Zipper a -> a
read (Zipper _ a _) = a

toList :: Zipper a -> Int -> [a]
toList (Zipper ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor Zipper where
  fmap f (Zipper l a r) = Zipper (fmap f l) (f a) (fmap f r)

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate z = Zipper (iterate' leftZipper z) z (iterate' rightZipper z)
    where
      iterate' f = tail . iterate f

{- -------------------------------------------------- -}
{- Z -}
data Z a = Z (Zipper (Zipper a))

upZ :: Z a -> Z a
upZ (Z z) = Z (leftZipper z)

downZ :: Z a -> Z a
downZ (Z z) = Z (rightZipper z)

leftZ :: Z a -> Z a
leftZ (Z z) = Z (fmap leftZipper z)

rightZ :: Z a -> Z a
rightZ (Z z) = Z (fmap rightZipper z)

instance Functor Z where
  fmap f (Z a) = Z $ (fmap . fmap) f a

instance Comonad Z where
  extract (Z a) = extract $ extract a
  duplicate z = Z $ fmap horizontal $ vertical z
    where
      horizontal = wrapDir leftZ rightZ
      vertical = wrapDir upZ downZ
      wrapDir a b e = Zipper (iterate' a e) e (iterate' b e)
      iterate' f = tail . iterate f

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

dispLine :: Zipper Bool -> String
dispLine z =
  map dispC $ toList z 6
  where
    dispC True = '*'
    dispC False = ' '

disp :: Z Bool -> String
disp (Z z) =
  unlines $ map dispLine $ toList z 6

glider :: Z Bool
glider =
  Z $ Zipper (repeat fz) fz rs
  where
    rs =
      [ line [f, t, f],
        line [f, f, t],
        line [t, t, t]
      ]
        ++ repeat fz
    t = True
    f = False
    fl = repeat f
    fz = Zipper fl f fl
    line l =
      Zipper fl f (l ++ fl)