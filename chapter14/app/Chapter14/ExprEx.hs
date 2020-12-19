module Chapter14.ExprEx where

data Expr a
  = Plus (Expr a) (Expr a)
  | Times (Expr a) (Expr a)
  | AmountOf a
  | Minus (Expr a) (Expr a)
  | DivideBy (Expr a) (Expr a)

foldExpr plusFn timesFn amountFn minusFn divideFn e =
  let f = foldExpr plusFn timesFn amountFn minusFn divideFn
   in case e of
        Plus e1 e2 -> plusFn (f e1) (f e2)
        Times e1 e2 -> timesFn (f e1) (f e2)
        AmountOf x -> amountFn x
        Minus e1 e2 -> minusFn (f e1) (f e2)
        DivideBy e1 e2 -> divideFn (f e1) (f e2)

meaning :: Eq a => [a] -> Expr a -> Int
meaning s = foldExpr (+) (*) (\x -> length $ filter (== x) s) (-) div

{-plusFn :: b -> b -> b
timesFn :: b -> b -> b
amountFn :: a -> b
minusFn :: b -> b -> b
divideFn :: b -> b -> b
-}
newtype ExprAlgebra a b =
  ExprAlgebra (b -> b -> b, b -> b -> b, a -> b, b -> b -> b, b -> b -> b)

foldExpr' :: ExprAlgebra a b -> Expr a -> b
foldExpr' a@(ExprAlgebra (plusFn, timesFn, amountFn, minusFn, divideFn)) e =
  case e of
    Plus e1 e2 -> plusFn (foldExpr' a e1) (foldExpr' a e2)
    Times e1 e2 -> timesFn (foldExpr' a e1) (foldExpr' a e2)
    AmountOf x -> amountFn x
    Minus e1 e2 -> minusFn (foldExpr' a e1) (foldExpr' a e2)
    DivideBy e1 e2 -> divideFn (foldExpr' a e1) (foldExpr' a e2)
