{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Chapter3.Section2.FnsParams where

import Chapter3.Section1.HigherOrder (Client(..), Person(..))

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map f xs

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

{- HLINT ignore equalTuples -}
equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x, y) -> x == y) t

{- HLINT ignore sayHello -}
sayHello :: [String] -> [String]
sayHello names =
  map
    (\name ->
       case name of
         "Alejandro" -> "Hello, writer"
         _ -> "Welcome, " ++ name)
    names

{- HLINT ignore sayHello' -}
sayHello' :: [String] -> [String]
sayHello' names =
  map
    (\case
       "Alejandro" -> "Hello, writer"
       name -> "Welcome, " ++ name)
    names

{- HLINT ignore multiplyByN -}
multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> x * n

--        {- Ex 3-2 -}
--
--
{- HLINT ignore filterOnes -}
filterOnes :: [Integer] -> [Integer]
filterOnes nums =
  filter
    (\case
       1 -> True
       _ -> False)
    nums

{- HLINT ignore filterANumber -}
filterANumber :: [Integer] -> Integer -> [Integer]
filterANumber nums a = filter (\n -> n == a) nums

{- HLINT ignore filterNot -}
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f x = filter (\n -> not $ f n) x

{- HLINT ignore filterGovOrgs -}
filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs clients =
  filter
    (\case
       GovOrg {..} -> True
       _ -> False)
    clients

isGovOrg :: Client a -> Bool
isGovOrg GovOrg {..} = True
isGovOrg _ = False

{- HLINT ignore filterGovOrgs' -}
filterGovOrgs' :: [Client a] -> [Client a]
filterGovOrgs' clients = filter isGovOrg clients

duplicateOdds list = map (* 2) $ filter odd list

duplicateOdds' = map (* 2) . filter odd
