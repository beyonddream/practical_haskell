{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.KMeans where

import Data.List
import qualified Data.Map as M
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
kMeans i k points threshold = kMeans' (i k points) points threshold 0

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
