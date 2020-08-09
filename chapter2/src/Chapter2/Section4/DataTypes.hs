{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Chapter2.Section4.DataTypes where

import Data.Char

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
  TimeMachine
    { manufacturer :: Manufacturer
    , model :: Model
    , name :: Name
    , direction :: Direction
    , price :: Price
    }
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
  Price
    { value :: Float
    }
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
  (case head timeMachines of
     t@TimeMachine {price = Price p} -> [t {price = Price (p - discountRate)}]) ++
  applyDiscount (tail timeMachines) discountRate

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
            {-
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
              in Just (f1 + f2) -}

ifibonacci :: Integer -> Maybe Integer
ifibonacci n
  | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n =
  let Just f1 = ifibonacci (n - 1)
      Just f2 = ifibonacci (n - 2)
   in Just (f1 + f2)

binom _ 0 = 1
binom x y
  | x == y = 1
binom n k = binom (n - 1) (k - 1) + binom (n - 1) k

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = mod x y == 0

specialMultiples :: Integer -> String
specialMultiples n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise = show n ++ " is a beautiful number"

ackerman :: Integer -> Integer -> Maybe Integer
ackerman 0 n
  | n > 0 = Just (n + 1)
ackerman m 0
  | m > 0 = ackerman (m - 1) 1
ackerman m n
  | m > 0 && n > 0 =
    let Just aRt = ackerman m (n - 1)
     in ackerman (m - 1) aRt
ackerman _ _ = Nothing

unzip1 :: [(Integer, Integer)] -> ([Integer], [Integer])
unzip1 [] = ([], [])
unzip1 [_] = ([], [])
unzip1 (x:y:xs) =
  let (x1, y1) = x
      (x2, y2) = y
   in ([x1, x2] ++ left, [y1, y2] ++ right)
  where
    (left, right) = unzip1 xs

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

clientName :: Client -> String
clientName client =
  case client of
    GovOrg name -> name
    Company name _ _ _ -> name
    Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

data ClientR
  = GovOrgR
      { clientRName :: String
      }
  | CompanyR
      { clientRName :: String
      , companyId :: Integer
      , person :: PersonR
      , duty :: String
      }
  | IndividualR
      { person :: PersonR
      }
  deriving (Show)

data PersonR =
  PersonR
    { firstName :: String
    , lastName :: String
    }
  deriving (Show)

greet :: ClientR -> String
greet IndividualR {person = PersonR {..}} = "Hi, " ++ firstName
greet CompanyR {..} = "Hi, " ++ clientRName
greet GovOrgR {} = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@PersonR {firstName = initial:rest} =
  let newName = toUpper initial : rest
   in p {firstName = newName}
nameInCapitals p@PersonR {firstName = ""} = p
