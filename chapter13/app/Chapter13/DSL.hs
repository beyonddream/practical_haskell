{-# LANGUAGE GADTs #-}

module Chapter13.DSL where

import Data.List

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

