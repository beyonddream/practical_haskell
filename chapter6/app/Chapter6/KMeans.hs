{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.KMeans where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Lens.Micro.Platform

class Ord v =>
      Vector v
  where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid lst =
    let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
        n = fromIntegral $ length lst
     in (u / n, v / n)

class Vector v =>
      Vectorizable e v
  where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

clusterAssignmentPhase ::
     (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) centroids
            in M.adjust (p :) chosenC m)
        initialMap
        points
  where
    compareDistance p x y =
      compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans ::
     (Ord v, Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int -- number of centroids
  -> [e] -- the information
  -> Double -- threshold
  -> ([v], Int) -- final centroids with no of iterations
kMeans i k points threshold = kMeans' (i k points) points threshold 1

kMeans' ::
     (Ord v, Vector v, Vectorizable e v)
  => [v]
  -> [e]
  -> Double
  -> Int
  -> ([v], Int)
kMeans' centroids points threshold depth =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
        then (newCentroids, depth)
        else kMeans' newCentroids points threshold (depth + 1)

-- test kmeans
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

fullName :: Lens' Person String
fullName =
  lens
    (\(Person f l) -> f ++ " " ++ l)
    (\_ newFullName ->
       case words newFullName of
         f:l:_ -> Person f l
         _ -> error "Incorrect name")

data Client i
  = GovOrg
      { _identifier :: i
      , _name :: String
      }
  | Company
      { _identifier :: i
      , _name :: String
      , _person :: Person
      , _duty :: String
      }
  | Individual
      { _identifier :: i
      , _person :: Person
      }
  deriving (Show)

data Person =
  Person
    { _firstName :: String
    , _lastName :: String
    }
  deriving (Show)

makeLenses ''Client

makeLenses ''Person

data TimeMachine =
  TimeMachine
    { _manufacturer :: Manufacturer
    , _model :: Model
    , _name' :: Name
    , _direction :: Direction
    , _price :: Price
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
    { _value :: Float
    }
  deriving (Show)

makeLenses ''TimeMachine

makeLenses ''Manufacturer

makeLenses ''Model

makeLenses ''Name

makeLenses ''Direction

makeLenses ''Price

makePremiumPrice :: [TimeMachine] -> Float -> [TimeMachine]
makePremiumPrice timeMachines premiumPercentage =
  timeMachines & traversed . price . value %~ (* (1 + premiumPercentage / 100))

data KMeansState e v =
  KMeansState
    { _centroids :: [v]
    , _points :: [e]
    , _err :: Double
    , _threshold :: Double
    , _steps :: Int
    }
  deriving (Show)

makeLenses ''KMeansState

initializeState ::
     (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0 / 0.0) t 0

clusterAssignmentPhase' ::
     (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase' state =
  let p = state ^. points
      c = state ^. centroids
      initialMap = M.fromList $ zip c (repeat [])
   in foldr
        (\p' m ->
           let chosenC = minimumBy (compareDistance p') c
            in M.adjust (p' :) chosenC m)
        initialMap
        p
  where
    compareDistance p x y =
      compare (distance x $ toVector p) (distance y $ toVector p)

{- HLINT ignore kMeans'' -}
kMeans'' ::
     (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans'' state =
  let assignments = clusterAssignmentPhase' state
      state1 =
        state & centroids . traversed %~
        (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
      state2 =
        state1 & err .~
        sum (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3 = state2 & steps +~ 1
   in if state3 ^. err < state3 ^. threshold
        then state3
        else kMeans'' state3
        {- HLINT ignore meanPurchase -}

meanPurchase ::
     Integer -- the client identifier
  -> Double -- the mean purchase
meanPurchase clientId =
  let p = purchaseByClientId clientId
      sum' = foldr (+) 0.0 $ catMaybes $ map purchaseValue p
      total' = foldr (+) 0 $ map numberOfItemsPurchased p
   in sum' / fromIntegral total'

numberOfItemsPurchased :: Integer -> Integer
numberOfItemsPurchased purchaseId =
  fromMaybe 0 (numberItemsByPurchaseId purchaseId)

purchaseByClientId :: Integer -> [Integer]
purchaseByClientId _ = [1, 2, 3]

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId _ = Just 3

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId _ = Just 4

priceByProductId :: Integer -> Maybe Double
priceByProductId _ = Just 5.0

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  case numberItemsByPurchaseId purchaseId of
    Nothing -> Nothing
    Just n ->
      case productIdByPurchaseId purchaseId of
        Nothing -> Nothing
        Just prId ->
          case priceByProductId prId of
            Nothing -> Nothing
            Just price' -> Just $ fromInteger n * price'

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

purchaseValue' :: Integer -> Maybe Double
purchaseValue' purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo`
  (\n ->
     productIdByPurchaseId purchaseId `thenDo`
     (\productId ->
        priceByProductId productId `thenDo`
        (\price'' -> Just $ fromInteger n * price'')))
