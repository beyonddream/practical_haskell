module Chapter7.MonadPlus where

import Control.Monad
import Data.List

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps year =
  nub $ do
    x <- [-1, 3, 5]
    y <- [-1, 3, 5]
    z <- [-1, 3, 5]
    return $ year + x + y + z

brokenJumps :: Int -> Int -> [Int]
brokenJumps year jumps =
  nub $ map (\x -> year + sum x) $ replicateM jumps [-1, 3, 5]

broken1 :: Integer -> [Integer]
broken1 n = [n - 1, n + 1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n + 2]
