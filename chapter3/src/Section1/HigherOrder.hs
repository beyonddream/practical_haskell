module HigherOrder where

maybeString (Just _) = "Just"
maybeString Nothing = "Nothing"

data Client i
  = GovOrg
      { clientId :: i
      , clientName :: String
      }
  | Company
      { clientId :: i
      , clientName :: String
      , person :: Person
      , duty :: String
      }
  | Individual
      { clientId :: i
      , person :: Person
      }
  deriving (Show, Eq, Ord)

data Person =
  Person
    { firstName :: String
    , lastName :: String
    }
  deriving (Show, Eq, Ord)

data Triple a b c =
  Triple a b c
  deriving (Show)

data SamePair a =
  SamePair a a
  deriving (Show)
