{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Chapter3.Section4.Lists where

import Chapter3.Section1.HigherOrder (Client(..), Person(..))
import Data.Function
import Data.List

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) =
  if f x
    then x : filter' f xs
    else filter' f xs

data InfNumber a
  = MinusInfinity
  | Number a
  | PlusInfinity
  deriving (Show)

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity _ = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ initial [] = initial
foldr'' f initial (x:xs) = f x (foldr'' f initial xs)

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ initial [] = initial
foldl'' f initial (x:xs) = foldl'' f (f initial x) xs

{- HLint Ignore maximum -}
maximum' :: [Integer] -> Integer
maximum' = foldr1 max

-- Ex 3-3
product' :: Num a => [a] -> a
product' [] = 1
product' [x] = x
product' (x:y:zs) = x * y * (product' zs)

minimumClient' :: [Client a] -> Int
minimumClient' [x] =
  case x of
    GovOrg {clientName} -> length clientName
    Company {clientName} -> length clientName
    Individual {person = Person {firstName}} -> length firstName
minimumClient' (x:xs) = min (minimumClient' [x]) (minimumClient' xs)

all' :: [Bool] -> Bool
all' [] = True
all' (x:xs) = x && all' xs

-- using foldr
product'' :: Num a => [a] -> a
product'' xs = foldr (*) 1 xs

all'' :: [Bool] -> Bool
all'' xs = foldr (&&) True xs

minimumClient'' :: [Client a] -> Int
minimumClient'' [] = 0
minimumClient'' (x:xs) = foldr (\x y -> min y (getLength x)) (getLength x) xs

-- using foldl
product''' :: Num a => [a] -> a
product''' xs = foldl (*) 1 xs

all''' :: [Bool] -> Bool
all''' xs = foldl (&&) True xs

minimumClient''' :: [Client a] -> Int
minimumClient''' [] = 0
minimumClient''' (x:xs) = foldl (\x y -> min x (getLength y)) (getLength x) xs

getLength client =
  case client of
    GovOrg {clientName} -> length clientName
    Company {clientName} -> length clientName
    Individual {person = Person {firstName}} -> length firstName

minimumBy f xs =
  foldr1
    (\x y ->
       if (f x) < (f y)
         then x
         else y)
    xs

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov =
  dropWhile
    (\case
       GovOrg {} -> False
       _ -> True)

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _ = False

checkAnalytics :: [Client a] -> (Bool, Bool)
checkAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

elem' n list =
  let r = find (== n) list
   in case r of
        Just _ -> True
        _ -> False

elem'' _ [] = False
elem'' n (x:xs) = n == x || elem'' n xs

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual {person = p1}) (Individual {person = p2}) =
  compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2 = compare (clientName c1) (clientName c2)

listOfClients =
  [ Individual 2 (Person "H. G." "Wells")
  , GovOrg 3 "NTTF" -- National Time Travel Foundation
  , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
  , Individual 5 (Person "Doctor" "")
  , Individual 6 (Person "Sarah" "Jane")
  ]

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics =
  map (duty . head) .
  sortBy (\x y -> compare (length y) (length x)) .
  groupBy (\x y -> duty x == duty y) . filter isCompany
  where
    isCompany (Company {}) = True
    isCompany _ = False

companyDutiesAnalytics' :: [Client a] -> [String]
companyDutiesAnalytics' =
  map (duty . head) .
  sortBy (flip (compare `on` length)) .
  groupBy ((==) `on` duty) . filter isCompany
  where
    isCompany (Company {}) = True
    isCompany _ = False

enum :: Int -> Int -> [Int]
enum a b
  | a > b = []
enum a b = a : enum (a + 1) b

withPositions :: [a] -> [(Int, a)]
withPositions list = zip (enum 1 $ length list) list

withPositions' :: [a] -> [(Int, a)]
withPositions' list = zip [1 .. length list] list
