module Main where

import Chapter8.ParEx
import Chapter8.STMEx
import Control.Concurrent
import qualified MyLib (someFunc)

main :: IO ()
main = do
  v <- newMVar 10000
  s <- newMVar [("a", 7)]
  forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
  forkDelay 5 $ printMoneyAndStock v s
  _ <- getLine -- to wait for completion
  return ()
