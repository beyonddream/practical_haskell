module Main where

import qualified MyLib6 (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib6.someFunc
