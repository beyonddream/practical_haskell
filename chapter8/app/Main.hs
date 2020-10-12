module Main where

import Chapter8.AmqpEx
import Chapter8.QEx
import Chapter8.STMEx
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import System.Environment
import System.IO
import System.Random

main :: IO ()
main = do
  conn <- initialize
  args <- getArgs
  case args of
    "backend":_ -> backend conn
    "frontend":_ -> do
      frontend conn
      _ <- getLine
      return ()
        {-main = do
  q <- newTBQueueIO 3
  travelYears <- newTVarIO []
  r <- randomRIO (1, 3)
  travelDuration <- newDelay (r * 1000000)
  _ <- forkIO $ timeMachineAllocator q
  _ <- forkIO $ timeMachineRequestor q "tm1" 2021 travelYears travelDuration
  _ <- forkIO $ timeMachineRequestor q "tm2" 2022 travelYears travelDuration
  _ <- forkIO $ timeMachineRequestor q "tm3" 2022 travelYears travelDuration
  _ <- forkIO $ timeMachineRequestor q "tm4" 2023 travelYears travelDuration
  _ <- getLine
  return ()-}
        {-main :: IO ()
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
  -}
