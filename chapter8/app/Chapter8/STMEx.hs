{-# LANGUAGE BlockArguments #-}

module Chapter8.STMEx where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import System.Random

updateMoney :: MVar Integer -> IO ()
updateMoney v = do
  m <- takeMVar v
  putStrLn $ "Updating value, which is " ++ show m
  putMVar v (m + 500) -- suppose a constant price

readMoney :: MVar Integer -> IO ()
readMoney v = do
  m <- readMVar v
  putStrLn $ "The current value is " ++ show m

randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (3, 15)
  threadDelay (r * 1000000)

forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)
        {- HLINT ignore updateMoneyAndStock -}

updateMoneyAndStock ::
     Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStock product' price money stock = do
  s <- readTVar stock
  let Just productNo = lookup product' s
  if productNo > 0
    then do
      m <- readTVar money
      let newS =
            map
              (\(k, v) ->
                 if k == product'
                   then (k, v - 1)
                   else (k, v))
              s
      writeTVar money (m + price) >> writeTVar stock newS
    else return ()

printMoneyAndStock :: Show a => TVar Integer -> TVar [(a, Integer)] -> IO ()
printMoneyAndStock money stock = do
  m <- readTVarIO money
  s <- readTVarIO stock
  putStrLn $ show m ++ "\n" ++ show s

performTimeTravel ::
     String
  -> Integer
  -> TVar Integer
  -> Integer
  -> TVar [Integer]
  -> Delay
  -> STM ()
performTimeTravel name year capacity maxCapacity travelYears wait = do
  startTravel name capacity maxCapacity travelYears year
  waitDelay wait
  endTravel name capacity travelYears year

startTravel ::
     String -> TVar Integer -> Integer -> TVar [Integer] -> Integer -> STM ()
startTravel name capacity maxCapacity travelYears year = do
  c <- readTVar capacity
  years <- readTVar travelYears
  when
    (c == maxCapacity || isJust (find (== year) years))
    (traceM ("retrying" ++ name) >> retry)
  writeTVar capacity (c + 1)
  writeTVar travelYears (insert year years)
  c' <- readTVar capacity
  y' <- readTVar travelYears
  traceM
    ("starting travel for : " ++
     show name ++ " " ++ show year ++ " " ++ show c' ++ " " ++ show y')

endTravel :: String -> TVar Integer -> TVar [Integer] -> Integer -> STM ()
endTravel name capacity travelYears year = do
  c <- readTVar capacity
  years <- readTVar travelYears
  writeTVar capacity (c - 1)
  writeTVar travelYears (delete year years)
  c' <- readTVar capacity
  y' <- readTVar travelYears
  traceM
    ("ending travel for: " ++
     show name ++ " " ++ show year ++ " " ++ show c' ++ " " ++ show y')
