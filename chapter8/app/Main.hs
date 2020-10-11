module Main where

import Chapter8.STMEx
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import System.IO
import System.Random

main :: IO ()
main = do
  capacity <- newTVarIO 0
  travelYears <- newTVarIO []
  r <- randomRIO (1, 3)
  travelDuration <- newDelay (r * 1000000)
  atomically $
    performTimeTravel "tm1" 2021 capacity 2 travelYears travelDuration
  atomically $
    performTimeTravel "tm2" 2022 capacity 2 travelYears travelDuration
  atomically $
    performTimeTravel "tm3" 2022 capacity 2 travelYears travelDuration
  _ <- getLine
  return ()
