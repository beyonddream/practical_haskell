module Chapter2.Section4.DataTypes where

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data Person =
  Person String String Gender
  deriving (Show)

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show)

data TimeMachine =
  TimeMachine Manufacturer Model Name Direction Price
  deriving (Show)

newtype Manufacturer =
  Manufacturer String
  deriving (Show)

newtype Model =
  Model Int
  deriving (Show)

newtype Name =
  Name String
  deriving (Show)

data Direction
  = PAST
  | FUTURE
  deriving (Show)

newtype Price =
  Price Float
  deriving (Show)

companyName :: Client -> Maybe String
companyName client =
  case client of
    Company name _ _ _ -> Just name
    _ -> Nothing

fibonacci :: Integer -> Integer
fibonacci n =
  case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci (n - 1) + fibonacci (n - 2)

-- Exercise 2-5
-- part a
clientsPerGender :: [Client] -> Gender -> Int -> Int
clientsPerGender clients gender acc =
  let count =
        case (getGender (head clients), gender) of
          (Male, Male) -> acc + 1
          (Female, Female) -> acc + 1
          (Unknown, Unknown) -> acc + 1
          (_, _) -> acc
   in if null (tail clients)
        then count
        else clientsPerGender (tail clients) gender count

getGender :: Client -> Gender
getGender (Company _ _ (Person _ _ gender) _) = gender
getGender (Individual (Person _ _ gender) _) = gender
getGender (GovOrg _) = Unknown

-- part b
applyDiscount :: [TimeMachine] -> Float -> [TimeMachine]
applyDiscount [] _ = []
applyDiscount timeMachines discountRate =
  applyDiscount (tail timeMachines) discountRate ++
  case head timeMachines of
    TimeMachine manufacturer model name direction (Price val) ->
      [ TimeMachine
          manufacturer
          model
          name
          direction
          (Price (val - val * discountRate))
      ]

null1 :: [a] -> Bool
null1 [] = True
null1 (_:_) = False

head1 :: [a] -> a
head1 [] = head [] -- using head to so it can throw exception
head1 (x:_) = x

tail1 :: [a] -> [a]
tail1 [] = []
tail1 (_:xs) = xs

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:r@(y:_)) = x < y && sorted r

maxmin :: [Integer] -> (Integer, Integer)
maxmin [x] = (x, x)
maxmin (x:xs) =
  ( if x > xs_max
      then x
      else xs_max
  , if x < xs_min
      then x
      else xs_min)
  where
    (xs_max, xs_min) = maxmin xs

ifibonacci :: Integer -> Maybe Integer
ifibonacci n =
  if n < 0
    then Nothing
    else case n of
           0 -> Just 0
           1 -> Just 1
           n' ->
             let Just f1 = ifibonacci (n' - 1)
                 Just f2 = ifibonacci (n' - 2)
              in Just (f1 + f2)
