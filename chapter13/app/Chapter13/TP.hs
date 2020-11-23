{-# LANGUAGE DataKinds, GADTs, TypeFamilies, UndecidableInstances
  #-}
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
  |])
