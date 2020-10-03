module Chapter8.ParEx where

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n =
  let oneFactor = findFactor n 2
   in oneFactor : findFactors (n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m
  | n == m = m
  | n `mod` m == 0 = m
  | otherwise = findFactor n (m + 1)

findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = (findFactors x, findFactors y)
