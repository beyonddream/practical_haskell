module Chapter14.AG where

data Expr
  = Plus Expr Expr
  | Times Expr Expr
  | AmountOf Char

{- HLINT ignore meaning -}
meaning :: Expr -> [Char] -> Int
meaning (Plus l r) p = meaning l p + meaning r p
meaning (Times l r) p = meaning l p * meaning r p
meaning (AmountOf c) p = length $ filter (== c) p
