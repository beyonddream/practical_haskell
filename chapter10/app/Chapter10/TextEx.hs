{-# LANGUAGE OverloadedStrings #-}

module Chapter10.TextEx where

import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T
import Data.Monoid
import Data.Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

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

data Purchase =
  Purchase
    { client :: Client Int
    , products :: [Product']
    }

instance Eq Person where
  Person firstNameL lastNameL == Person firstNameR lastNameR =
    firstNameL == firstNameR && lastNameL == lastNameR

saveClients :: FilePath -> [Client Int] -> IO ()
saveClients fpath clients =
  runConduitRes $
  L.sourceList clients .| L.map clientToText .|
  L.map (LT.toStrict . B.toLazyText) .|
  L.concatMap (\x -> [x, "\n"]) -- write '\n' between clients
   .|
  T.encode T.utf8 .|
  B.sinkFile fpath

clientToText :: Client Int -> B.Builder
clientToText (GovOrg i n) =
  "client(gov," <>
  B.decimal i <>
  B.singleton ',' <> B.fromText (escapeString n) <> B.singleton ')'
clientToText (Company i n p d) =
  "client(com," <>
  B.decimal i <>
  B.singleton ',' <>
  B.fromText (escapeString n) <>
  B.singleton ',' <>
  personToText p <>
  B.singleton ',' <> B.fromText (escapeString d) <> B.singleton ')'
clientToText (Individual i p) =
  "client(ind," <>
  B.decimal i <> B.singleton ',' <> personToText p <> B.singleton ')'

personToText :: Person -> B.Builder
personToText (Person f l) =
  "person(" <>
  B.fromText (escapeString f) <>
  B.singleton ',' <> B.fromText (escapeString l) <> B.singleton ')'

purchaseToText :: Purchase -> B.Builder
purchaseToText (Purchase c p) =
  "purchase(" <>
  clientToText c <>
  B.singleton ',' <>
  B.singleton '[' <> productsToText p <> B.singleton ']' <> B.singleton ')'

productsToText :: [Product'] -> B.Builder
productsToText [] = ""
productsToText ((Product' i n p d):xs) =
  "product(" <>
  B.decimal i <>
  B.singleton ',' <>
  B.fromText (escapeString n) <>
  B.singleton ',' <>
  B.realFloat p <>
  B.singleton ',' <>
  B.fromText (escapeString d) <>
  B.singleton ')' <> B.singleton ',' <> productsToText xs

escapeString :: String -> Text
escapeString =
  replace "\n" "\\n" .
  replace "," "\\," .
  replace "(" "\\(" .
  replace ")" "\\)" . replace "[" "\\[" . replace "]" "\\]" . pack
