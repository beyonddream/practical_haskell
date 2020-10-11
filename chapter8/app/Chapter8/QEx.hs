module Chapter8.QEx where

import Chapter8.STMEx
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

main :: IO ()
main = do
  q <- newTQueueIO
  _ <- forkIO $ backend q
  replicateM_ 10 $ forkIO (frontend q)
  _ <- getLine
  return ()

backend :: TQueue (String, Integer) -> IO ()
backend q = do
  m <- newTVarIO 10000
  s <- newTVarIO [("a", 7)]
  forever $
    atomically $ do
      (product', price) <- readTQueue q
      updateMoneyAndStock product' price m s

frontend :: TQueue (String, Integer) -> IO ()
frontend q = do
  (product'', price) <- undefined
  atomically $ writeTQueue q (product'', price)
