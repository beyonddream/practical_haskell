{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.StateLenses where

import Control.Monad (unless)
import Control.Monad.RWS
import Control.Monad.ST
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Monoid (Sum(..))
import Data.STRef
import Lens.Micro.Platform

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

fullName :: Lens' Person String
fullName =
  lens
    (\(Person f l) -> f ++ " " ++ l)
    (\_ newFullName ->
       case words newFullName of
         f:l:_ -> Person f l
         _ -> error "Incorrect name")

makeLenses ''Client

data ExampleSt =
  ExampleSt
    { _increment :: Int
    , _clients :: [Client Int]
    }
  deriving (Show)

makeLenses ''ExampleSt

zoomCl :: State ExampleSt ()
zoomCl = do
  n <- use increment
  zoom (clients . traversed) $ do
    identifier += n
    person . fullName %= map toUpper

newtype MyWriter m a =
  MyWriter (a, m)

instance Monoid m => Monad (MyWriter m) where
  return a = MyWriter (a, mempty)
  (>>=) (MyWriter (a, m1)) g =
    let (MyWriter (b, m2)) = g a
     in MyWriter (b, m1 <> m2)

instance Monoid m => Functor (MyWriter m) where
  fmap = liftM

instance Monoid m => Applicative (MyWriter m) where
  pure = return
  (<*>) = ap

--tell :: Monoid m => m -> MyWriter m ()
--tell m = MyWriter ((), m)
{- HLINT ignore listLength -}
listLength :: [a] -> Integer
listLength list =
  runST $ do
    l <- newSTRef 0
    traverseList list l
    readSTRef l
  where
    traverseList [] _ = return ()
    traverseList (_:xs) l = do
      _ <- modifySTRef' l (+ 1)
      traverseList xs l

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

newCentroids' :: (Ord v, Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids' = M.elems . fmap (centroid . map toVector)

clusterAssignments ::
     (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points' =
  let initialMap = M.fromList $ zip centrs (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) centrs
            in M.adjust (p :) chosenC m)
        initialMap
        points'
  where
    compareDistance p x y =
      compare (distance x $ toVector p) (distance y $ toVector p)

kMeans' :: (Vector v, Vectorizable e v) => [e] -> RWS Double (Sum Int) [v] ()
kMeans' points = do
  prevCentrs <- get
  let assignments = clusterAssignments prevCentrs points
      newCentrs = newCentroids' assignments
  put newCentrs
  tell (Sum 1)
  t <- ask
  let err = sum $ zipWith distance prevCentrs newCentrs
  unless (err < t) $ kMeans' points

kMeans ::
     (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])
  -> Int
  -> [e]
  -> Double
  -> ([v], Sum Int)
kMeans i n pts t = execRWS (kMeans' pts) t (i n pts)
