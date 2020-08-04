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

clientName :: Client -> String
clientName client =
  case client of
    GovOrg name -> name
    Company name _ _ _ -> name
    Individual person _ ->
      case person of
        Person fNm lNm _ -> fNm ++ " " ++ lNm
