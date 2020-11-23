{-# LANGUAGE EmptyDataDecls, GADTs, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Chapter13.DSL where

import Data.List

data AllowEverything
data AllowProducts
data AllowPurchases

data Expr a r where
  AmountOf :: a -> Expr a Integer
  PriceOf :: a -> Expr a Float
  TotalNumberProducts :: Expr a Integer
  TotalPrice :: Expr a Float
  IVal :: Integer -> Expr a Integer
  FVal :: Float -> Expr a Float
  (:+:) :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:) :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:<=:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:>:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:>=:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:&&:) :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:) :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not :: Expr a Bool -> Expr a Bool

interpretExpr :: Eq a => Expr a t -> [(a, Float)] -> t
interpretExpr (e1 :+: e2) list
  = interpretExpr e1 list + interpretExpr e2 list
interpretExpr (e1 :*: e2) list
  = interpretExpr e1 list * interpretExpr e2 list
interpretExpr (e1 :||: e2) list
  = interpretExpr e1 list || interpretExpr e2 list
interpretExpr (e1 :&&: e2) list
  = interpretExpr e1 list && interpretExpr e2 list
interpretExpr (Not e) list
  = not $ interpretExpr e list
interpretExpr (IVal x) _ = x
interpretExpr (FVal x) _ = x
interpretExpr TotalNumberProducts list = toInteger (length $ nub $ [ x | (x,y) <- list])
interpretExpr TotalPrice list = sum $ [y | (x, y) <- list]
interpretExpr (PriceOf a) list = head $ [ p | (n, p) <- list , n == a]
interpretExpr (e1 :<: e2) list = interpretExpr e1 list < interpretExpr e2 list
interpretExpr (e1 :<=: e2) list = interpretExpr e1 list <= interpretExpr e2 list
interpretExpr (e1 :>: e2) list = interpretExpr e1 list > interpretExpr e2 list
interpretExpr (e1 :>=: e2) list = interpretExpr e1 list >= interpretExpr e2 list

data Person = Person { firstName :: String, lastName :: String }

data User r where
        Admin :: Person -> User AllowEverything
        StoreManager :: Person -> User AllowEverything
        StorePerson :: Person -> User AllowProducts
        Client :: Person -> User AllowPurchases

data NoPork
data Vegetarian
data LowSalt
data Pork

data Snack t where
        BaconBites :: Snack Pork
        Cracker :: Snack NoPork
        Popcorn :: Snack Vegetarian
        RoastedZucchini :: Snack LowSalt

data Number = Zero | Succ Number deriving Show

one :: Number
one = Succ Zero

two :: Number
two = Succ one

min' :: Number -> Number -> Number
min' Zero _ = Zero
min' _ Zero = Zero
min' (Succ x) (Succ y) = Succ (min' x y)

class Product p b op | p -> op, op -> b, b -> p where
        price :: p -> Float
        perform :: p -> op -> String
        testOperation :: p -> op

{- HLINT ignore -}
data TimeMachine = TimeMachine { model :: String } deriving Show

data TimeMachineOps = Travel Integer | Park deriving Show

instance Product TimeMachine BigBag TimeMachineOps where
        price _ = 1000.0
        perform (TimeMachine m) (Travel y)
          = "Travelling to " ++ show y ++ " with " ++ m
        perform (TimeMachine m) Park
          = "Parking time machine " ++ m
        testOperation _ = Travel 0

totalAmount :: Product p op b => [p] -> Float
totalAmount = foldr (+) 0.0 . map price

performTest :: Product p op b => p -> String
performTest p = perform p $ testOperation p

data BigBag
data SmallBag

data Book = Book { title :: String, author :: String, rating :: Integer }
        deriving Show

data BookOps = Read | Highlight | WriteCritique deriving Show

instance Product Book SmallBag BookOps where
        price _ = 500.0
        perform (Book { title, author, rating }) Read =
                "Reading book " ++ title ++ " written by author " ++  author ++
                        ". It has rating of " ++ show rating
        perform (Book { title, author, rating }) WriteCritique =
                "The book " ++ title ++ " is a keeper!"
        perform (Book { title, author, rating }) Highlight =
                "Go to page 42 in the book " ++ title
        testOperation _ = Read

data Zero
data Succ n

data Vect n a where
        VNil :: Vect Zero a
        VCons :: a -> Vect n a -> Vect (Succ n) a

headVect :: Vect (Succ n) a -> a
headVect (VCons x _) = x

class Plus x y z | x y -> z

instance Plus Zero x x
instance Plus x y z => Plus (Succ x) y (Succ z)

data BinaryTree h n where
        Leaf :: n -> BinaryTree Zero n
        Node :: Max x y z => n -> BinaryTree x n1 -> BinaryTree y n2 -> BinaryTree z n

class Max x y z | x y -> z

instance Max Zero x x
instance Max x y z => Max (Succ x) (Succ y) (Succ z)
