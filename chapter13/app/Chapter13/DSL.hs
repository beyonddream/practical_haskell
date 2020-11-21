module Chapter13.DSL where

data Offer a
  = Present a
  | PercentDiscount Float
  | AbsoluteDiscount Float
  | Restrict [a] (Offer a)
             -- product restriction (1)
  | From Integer (Offer a)
             -- time restriction (2)
  | Until Integer (Offer a)
  | Extend Integer (Offer a)
  | Both (Offer a) (Offer a)
             -- offer combinators (3)
  | BetterOf (Offer a) (Offer a)
  | If (Expr a) (Offer a) (Offer a)
  deriving (Show)

data Expr a
  = AmountOf a
  | PriceOf a
        -- information about the cart
  | TotalNumberProducts
  | TotalPrice
        -- lifting numerical values
  | IVal Integer
  | FVal Float
        -- arithmetic
  | (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
        -- comparison
  | (Expr a) :<: (Expr a)
  | (Expr a) :<=: (Expr a)
  | (Expr a) :>: (Expr a)
  | (Expr a) :>=: (Expr a)
        -- boolean operations
  | (Expr a) :&&: (Expr a)
  | (Expr a) :||: (Expr a)
  | Not (Expr a)
  deriving (Show)

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

v :: Offer String
v =
  Until 30 $
  BetterOf
    (AbsoluteDiscount 10.0)
    (Both
       (Present "balloon")
       (If (TotalPrice :>: IVal 100) (PercentDiscount 5.0) noOffer))

-- 13-1
period :: Integer -> Integer -> Offer a -> Offer a
period f d o = Both (From f o) (Until (f + d) o)

allOf :: [Offer a] -> Offer a
allOf [] = noOffer
allOf [x] = x
allOf (x:xs) = Both x (allOf xs)
        {- HLINT ignore v131 -}

v131 :: Offer String
v131 =
  period
    3
    5
    (allOf
       [ (Present "balloon")
       , (Present "chocolate muffin")
       , (PercentDiscount 10.0)
       ])
