module Main where

import Chapter8.ParEx
import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  print $
    printTicket
      2
      3
      [(1, "Client1"), (2, "Client2")]
      [(3, "product3"), (5, "product5")]
  let info = [(1, 1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
   in print $ kMeans initializeSimple 2 info 0.001
