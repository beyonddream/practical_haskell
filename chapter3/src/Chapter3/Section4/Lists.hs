{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Chapter3.Section4.Lists where

import Chapter3.Section1.HigherOrder (Client(..), Person(..))

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
