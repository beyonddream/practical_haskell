module Chapter8.STMEx where

import Control.Concurrent
import Control.Monad
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
