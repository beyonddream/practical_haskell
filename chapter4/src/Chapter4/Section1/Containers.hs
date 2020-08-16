{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns #-}

module Chapter4.Section1.Containers where

import Data.Graph
import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree

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
  deriving (Show, Eq, Ord, Read)

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

---------
preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
  let subtreesTraversed = concatMap (preOrder f) subtrees
   in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [Node 2 [Node 3 [], Node 4 [], Node 5 []], Node 6 []]

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [ ("wood", "wood", ["walls"])
  , ("plastic", "plastic", ["walls", "wheels"])
  , ("aluminum", "aluminum", ["wheels", "door"])
  , ("walls", "walls", ["done"])
  , ("wheels", "wheels", ["done"])
  , ("door", "door", ["done"])
  , ("done", "done", [])
  ]

timeMachinePrecedence ::
     (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineTravel :: Graph
timeMachineTravel =
  buildG
    (103, 2013)
    [ (1302, 1614)
    , (1614, 1302)
    , (1302, 2013)
    , (2013, 1302)
    , (1614, 2013)
    , (2013, 1408)
    , (1408, 1993)
    , (1408, 917)
    , (1993, 917)
    , (917, 103)
    , (103, 917)
    ]

class Nameable n where
  name' :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name' n)

--
instance Nameable (Client i) where
  name' Individual {person = Person {firstName = f, lastName = n}} =
    f ++ " " ++ n
  name' c = clientName c

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

class Priceable n where
  priceOf :: n -> Double

instance Priceable TimeMachine where
  priceOf TimeMachine {price = Price {value}} = realToFrac value

totalPrice :: Priceable p => [p] -> Double
totalPrice = foldr (\x y -> priceOf x + y) 0.0
