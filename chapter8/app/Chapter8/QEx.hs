module Chapter8.QEx where

import Chapter8.STMEx
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import System.Random
        {-main :: IO ()
main = do
  q <- newTQueueIO
  _ <- forkIO $ backend q
  replicateM_ 10 $ forkIO (frontend q)
  _ <- getLine
  return ()-}
        {-backend :: TQueue (String, Integer) -> IO ()
backend q = do
  m <- newTVarIO 10000
  s <- newTVarIO [("a", 7)]
  forever $
    atomically $ do
      (product', price) <- readTQueue q
      return ()

frontend :: TQueue (String, Integer) -> IO ()
frontend q = do
  (product'', price) <- undefined
  atomically $ writeTQueue q (product'', price)-}

timeMachineRequestor ::
     TBQueue (String, Integer, TVar [Integer], Delay)
  -> String
  -> Integer
  -> TVar [Integer]
  -> Delay
  -> IO ()
timeMachineRequestor q name year travelYears travelDuration =
  atomically $ writeTBQueue q (name, year, travelYears, travelDuration)

timeMachineAllocator ::
     TBQueue (String, Integer, TVar [Integer], Delay) -> IO ()
timeMachineAllocator q =
  forever $
  atomically $ do
    (name, year, travelYears, wait) <- readTBQueue q
    _ <- startTravel' name travelYears year
    _ <- waitDelay wait
    _ <- endTravel' name travelYears year
    return ()

startTravel' :: String -> TVar [Integer] -> Integer -> STM ()
startTravel' name travelYears year = do
  years <- readTVar travelYears
  when (isJust (find (== year) years)) (traceM ("retrying" ++ name) >> retry)
  writeTVar travelYears (insert year years)
  y' <- readTVar travelYears
  traceM
    ("starting travel for : " ++ show name ++ " " ++ show year ++ " " ++ show y')

endTravel' :: String -> TVar [Integer] -> Integer -> STM ()
endTravel' name travelYears year = do
  years <- readTVar travelYears
  writeTVar travelYears (delete year years)
  y' <- readTVar travelYears
  traceM
    ("ending travel for: " ++
     show name ++ " " ++ show year ++ " " ++ " " ++ show y')
