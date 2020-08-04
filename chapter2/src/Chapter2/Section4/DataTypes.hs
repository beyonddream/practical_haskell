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

clientsPerGender :: [Client] -> Gender -> Int
clientsPerGender clients gender =
  let count =
        case (getGender (head clients), gender) of
          (Male, Male) -> 1
          (Female, Female) -> 1
          (Unknown, Unknown) -> 1
          (_, _) -> 0
   in if null (tail clients)
        then count
        else count + clientsPerGender (tail clients) gender

getGender :: Client -> Gender
getGender (Company _ _ (Person _ _ gender) _) = gender
getGender (Individual (Person _ _ gender) _) = gender
getGender (GovOrg _) = Unknown
