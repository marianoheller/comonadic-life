module Main where

import Lib


glider :: Z Bool
glider =
  Z $ LZ (repeat fz) fz rs
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
    fz = LZ fl f fl
    line l =
      LZ fl f (l ++ fl)

main :: IO ()
main = putStrLn "asd"
