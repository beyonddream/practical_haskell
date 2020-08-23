module Chapter5.Infinite where

data TimeMachine =
  TM
    { manufacturer :: String
    , year :: Integer
    }
  deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y + 1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

allNumbers :: [Integer]
allNumbers = allNumbersFrom 1

allNumbersFrom :: Integer -> [Integer]
allNumbersFrom n = n : allNumbersFrom (n + 1)

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

infinite2020Machines :: [TimeMachine]
infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines

specialOffer :: [TimeMachine]
specialOffer = cycle [TM m 2005, TM m 1994, TM m 908]
  where
    m = "Timely Inc."

fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n, n1) -> (n1, n + n1)) (0, 1)

primes :: [Integer]
primes =
  map head $ iterate (\x -> filter (\y -> y `mod` head x /= 0) (tail x)) [2 ..]

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
  where
    sumForce' [] z = z
    sumForce' (y:ys) z =
      let s = z + y
       in s `seq` sumForce' ys s
