module Main where

import Data.List

main :: IO ()
{- HLINT ignore main -}
main = putStrLn $ show result

result :: Integer
{- HLINT ignore result -}
result = foldl' (*) 1 [1 .. 100000]
