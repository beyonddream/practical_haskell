{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Chapter4.Section1.Containers where

import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v =
  M.alter
    (\case
       Nothing -> Just v
       (Just _) -> Just v)
    k

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete =
  M.alter
    (\case
       (Just _) -> Nothing)

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f =
  M.alter
    (\case
       (Just v) -> Just (f v))

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

data ClientKind
  = GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Show, Eq, Ord)

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients =
  foldr
    (\client m ->
       case client of
         GovOrg {..} ->
           M.insert
             GovOrgKind
             (S.insert client (M.findWithDefault S.empty GovOrgKind m))
             m
         Company {..} ->
           M.insert
             CompanyKind
             (S.insert client (M.findWithDefault S.empty CompanyKind m))
             m
         Individual {..} ->
           M.insert
             IndividualKind
             (S.insert client (M.findWithDefault S.empty IndividualKind m))
             m)
    M.empty

classifyClients' ::
     [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' clients =
  let listOfClientByKind =
        foldr
          (\client l ->
             case client of
               GovOrg {..} -> (GovOrgKind, S.singleton client) : l
               Company {..} -> (CompanyKind, S.singleton client) : l
               Individual {..} -> (IndividualKind, S.singleton client) : l)
          []
          clients
   in M.fromListWith S.union listOfClientByKind

clients :: Int -> Int -> [Client Integer]
clients count seed =
  zipWith assignId (unfoldr (Just . client) (mkStdGen seed)) [1 .. count]

client :: RandomGen g => g -> (Client Integer, g)
client g =
  case randomR (0 :: Int, 2) g of
    (0, g') -> (defaultGovOrg, g')
    (1, g') -> (defaultCompany, g')
    (_, g') -> (defaultIndividual, g')

assignId :: Client Integer -> Int -> Client Integer
assignId c i = c {clientId = toInteger i}

defaultGovOrg :: Client Integer
defaultGovOrg = GovOrg 0 "govorg"

defaultCompany :: Client Integer
defaultCompany = Company 0 "company" defaultPerson "duty"

defaultIndividual :: Client Integer
defaultIndividual = Individual 0 defaultPerson

defaultPerson :: Person
defaultPerson = Person "fn" "ln"
