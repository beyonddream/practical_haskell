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

updateMoneyAndStock ::
     Eq a => a -> Integer -> MVar Integer -> MVar [(a, Integer)] -> IO ()
updateMoneyAndStock product' price money stock = do
  s <- takeMVar stock
  let Just productNo = lookup product' s
  if productNo > 0
    then do
      m <- takeMVar money
      let newS =
            map
              (\(k, v) ->
                 if k == product'
                   then (k, v - 1)
                   else (k, v))
              s
      putMVar money (m + price) >> putMVar stock newS
    else putMVar stock s

printMoneyAndStock :: Show a => MVar Integer -> MVar [(a, Integer)] -> IO ()
printMoneyAndStock money stock = do
  m <- readMVar money
  s <- readMVar stock
  putStrLn $ show m ++ "\n" ++ show s
