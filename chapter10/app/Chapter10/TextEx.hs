{-# LANGUAGE OverloadedStrings #-}

module Chapter10.TextEx where

import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T
import Data.Monoid
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

instance Eq Person where
  Person firstNameL lastNameL == Person firstNameR lastNameR =
    firstNameL == firstNameR && lastNameL == lastNameR

saveClients :: FilePath -> [Client Int] -> IO ()
saveClients fpath clients =
  runConduitRes $
  L.sourceList clients .| L.map clientToText .|
  L.concatMap (\x -> [x, "\n"]) -- write '\n' between clients
   .|
  T.encode T.utf8 .|
  B.sinkFile fpath

clientToText :: Client Int -> Text
clientToText (GovOrg i n) =
  "client(gov," <> escapeString (show i) <> "," <> escapeString n <> ")"
clientToText (Company i n p d) =
  "client(com," <>
  escapeString (show i) <>
  "," <> escapeString n <> "," <> personToText p <> "," <> escapeString d <> ")"
clientToText (Individual i p) =
  "client(ind," <> escapeString (show i) <> "," <> personToText p <> ")"

personToText :: Person -> Text
personToText (Person f l) =
  "person(" <> escapeString f <> "," <> escapeString l <> ")"

escapeString :: String -> Text
escapeString =
  replace "\n" "\\n" .
  replace "," "\\," . replace "(" "\\(" . replace ")" "\\)" . pack
