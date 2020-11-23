{-# LANGUAGE DataKinds, GADTs, TypeFamilies, UndecidableInstances,
  TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter13.TP where

import Data.Singletons.TH hiding (Min)

{-- data Nat = Zero | Succ Nat

data Vect n a where
        VNil :: Vect Zero a
        VCons :: a -> Vect n a -> Vect (Succ n) a

--type family Plus (x :: Nat) (y :: Nat) :: Nat where
type family Plus x y where
        Plus Zero x = x
        Plus (Succ x) y = Succ (Plus x y)--}
$(promote
    [d|
  
  data Nat = Zero
           | Succ Nat
               deriving (Show, Eq)
  
  plus :: Nat -> Nat -> Nat
  plus Zero y = y
  plus (Succ x) y = Succ (plus x y)
  
  min :: Nat -> Nat -> Nat
  min Zero _ = Zero
  min _ Zero = Zero
  min (Succ x) (Succ y) = Succ (min x y)
  
  data Range = Empty
             | Open Nat
             | Closed Nat Nat
  
  infinite :: Range
  infinite = Open Zero
  
  data Comparison = Less'
                  | Equal'
                  | Greater'
  
  compare' :: Nat -> Nat -> Comparison
  compare' Zero Zero = Equal'
  compare' Zero (Succ _) = Less'
  compare' (Succ _) Zero = Greater'
  compare' (Succ x) (Succ y) = compare' x y
  
  restrict :: Range -> Range -> Range
  restrict Empty Empty = Empty
  restrict (Open f) r = restrictFrom f r
  restrict l (Open g) = restrictTo g l
  
  restrictFrom :: Nat -> Range -> Range
  restrictFrom _ Empty = Empty
  restrictFrom n (Open f) = restrictFrom1 n f (compare' n f)
  restrictFrom n (Closed f t)
    = restrictFrom2 n f t (compare' n f) (compare' n t)
  
  restrictFrom1 :: Nat -> Nat -> Comparison -> Range
  restrictFrom1 n _ Greater' = Open n
  restrictFrom1 _ f Equal' = Open f
  restrictFrom1 _ f Less' = Open f
  
  restrictFrom2 ::
                Nat -> Nat -> Nat -> Comparison -> Comparison -> Range
  restrictFrom2 _ _ _ Greater' Greater' = Empty
  restrictFrom2 _ _ _ Greater' Equal' = Empty
  restrictFrom2 n _ t Greater' Less' = Closed n t
  restrictFrom2 _ f t Equal' _ = Closed f t
  restrictFrom2 _ f t Less' _ = Closed f t
  
  restrictTo :: Nat -> Range -> Range
  restrictTo n Empty = Open n
  restrictTo n (Open f) = restrictTo1 n f (compare' n f)
  restrictTo n (Closed f t)
    = restrictTo2 n f t (compare' n f) (compare' n t)
  
  restrictTo1 :: Nat -> Nat -> Comparison -> Range
  restrictTo1 n f Greater' = Closed f n
  restrictTo1 n f Equal' = Closed f n
  restrictTo1 _ f Less' = Empty
  
  restrictTo2 ::
              Nat -> Nat -> Nat -> Comparison -> Comparison -> Range
  restrictTo2 n _ t Greater' Greater' = Closed t n
  restrictTo2 n f _ Greater' Equal' = Closed f n
  restrictTo2 n f _ Greater' Less' = Closed f n
  restrictTo2 n f _ Equal' _ = Closed f n
  restrictTo2 _ f t Less' _ = Empty
  |])

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

toNat :: SNat n -> Nat
toNat SZero = Zero
toNat (SSucc n) = Succ (toNat n)

printDateRestriction :: Offer a r -> String
printDateRestriction (From n _) = "From " ++ show (toNat n)
printDateRestriction (Until n _) = "Until " ++ show (toNat n)
printDateRestriction _ = "No date restriction"

data Offer a (r :: Range) where
  Present :: a -> Offer a Infinite
  PercentDiscount :: Float -> Offer a Infinite
  AbsoluteDiscount :: Float -> Offer a Infinite
  From :: SNat n -> Offer a d -> Offer a (RestrictFrom n d)
  Until :: SNat n -> Offer a d -> Offer a (RestrictTo n d)
  Restrict :: [a] -> Offer a Infinite
  Both :: Offer a d1 -> Offer a d2 -> Offer a (Restrict d1 d2)
  BetterOf :: Offer a d1 -> Offer a d2 -> Offer a (Restrict d1 d2)
  If :: Expr a n -> Offer a p -> Offer a (Restrict Infinite p)

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

