module Main where

import Control.Monad.State.Lazy
import Lib
import System.Console.ANSI
import Control.Concurrent ( threadDelay )


reportState :: Z Bool -> IO ()
reportState z = do
  clearScreen
  putStrLn $ disp z
  return ()

game :: StateT (Z Bool) IO ()
game = do
  z1 <- get
  liftIO $ reportState z1
  modify evolve
  z2 <- get
  liftIO $ threadDelay 1000000
  case z1 == z2 of
    True -> return ()
    False -> game

main :: IO ()
main = do
  grid <- makeRandomZ 20
  evalStateT game grid
  return ()
