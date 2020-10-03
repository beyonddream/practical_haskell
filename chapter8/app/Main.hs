module Main where

import Chapter8.ParEx
import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  print $ findTwoFactors' 7 10
