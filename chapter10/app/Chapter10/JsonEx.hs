{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Chapter10.JsonEx where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import Data.Text

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
  deriving (Show, Read)

data Person =
  Person
    { firstName :: String
    , lastName :: String
    }
  deriving (Show, Ord, Read)

data Product' =
  Product'
    { id :: Int
    , name :: String
    , price :: Double
    , description :: String
    }
  deriving (Show, Eq, Ord, Read)

data Purchase =
  Purchase
    { client :: Client Int
    , products :: [Product']
    }
  deriving (Show, Read)

instance Eq Person where
  Person firstNameL lastNameL == Person firstNameR lastNameR =
    firstNameL == firstNameR && lastNameL == lastNameR

jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) =
  case M.lookup "type" o of
    Just (String "govorg") -> GovOrg <$> o .: "id" <*> o .: "name"
    Just (String "company") ->
      Company <$> o .: "id" <*> o .: "name" <*> o .: "person" <*> o .: "duty"
    Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
    _ -> Control.Applicative.empty
jsonToClient _ = Control.Applicative.empty

instance ToJSON (Client Integer) where
  toJSON = clientToJSON

instance FromJSON i => FromJSON (Client i) where
  parseJSON = jsonToClient

personToJSON :: Person -> Value
personToJSON (Person f l) =
  object ["first" .= String (pack f), "last" .= String (pack l)]

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) =
  object
    [ "type" .= String "govorg"
    , "id" .= Number (fromInteger i)
    , "name" .= String (pack n)
    ]
clientToJSON (Company i n p d) =
  object
    [ "type" .= String "company"
    , "id" .= Number (fromInteger i)
    , "name" .= String (pack n)
    , "person" .= personToJSON p
    , "duty" .= String (pack d)
    ]
clientToJSON (Individual i p) =
  object
    [ "type" .= String "individual"
    , "id" .= Number (fromInteger i)
    , "person" .= personToJSON p
    ]

jsonToPerson :: Value -> Parser Person
jsonToPerson (Object o) = Person <$> o .: "first" <*> o .: "last"
jsonToPerson _ = Control.Applicative.empty

instance ToJSON Person where
  toJSON = personToJSON

instance FromJSON Person where
  parseJSON = jsonToPerson
