{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter8.ParEx where

import Control.DeepSeq
import Control.Monad.Par
import Data.List
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n =
  let oneFactor = findFactor n 2
   in oneFactor : findFactors (n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m
  | n == m = m
  | n `mod` m == 0 = m
  | otherwise = findFactor n (m + 1)

findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = (findFactors x, findFactors y)

findTwoFactors' :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors' x y =
  runPar $ do
    factorsXVar <- spawnP $ findFactors x
    let factorsY = findFactors y
        _ = rnf factorsY
    factorsX <- get factorsXVar
    return (factorsX, factorsY)

-- run this using `stack run -- +RTS -N2` N is number of cores to be used. Leave out for GHC to figure out itself.
-- Also, add this ghc options in the cabal file : ghc-options: -threaded
printTicket :: Int -> Int -> [(Int, String)] -> [(Int, String)] -> String
printTicket idC idP clients products' =
  runPar $ do
    clientV <- new
    productV <- new
    fork $ lookupPar clientV idC clients
    fork $ lookupPar productV idP products'
    envV <- new
    letterV <- new
    fork $ printEnvelope clientV envV
    fork $ printLetter clientV productV letterV
    envS <- get envV
    letterS <- get letterV
    return $ envS ++ "\n\n" ++ letterS

lookupPar :: (Eq a, NFData b) => IVar (Maybe b) -> a -> [(a, b)] -> Par ()
lookupPar i _ [] = put i Nothing
lookupPar i x ((k, v):r)
  | x == k = put i $ Just v
  | otherwise = lookupPar i x r

printEnvelope :: IVar (Maybe String) -> IVar String -> Par ()
printEnvelope clientV envV = do
  clientName' <- get clientV
  case clientName' of
    Nothing -> put envV "Unknown"
    Just n -> put envV $ "To: " ++ n

printLetter ::
     IVar (Maybe String) -> IVar (Maybe String) -> IVar String -> Par ()
printLetter clientV productV letterV = do
  clientName'' <- get clientV
  productName <- get productV
  case (clientName'', productName) of
    (Nothing, Nothing) -> put letterV "Unknown"
    (Just n, Nothing) -> put letterV $ n ++ " bought something"
    (Nothing, Just p) -> put letterV $ "Someone bought " ++ p
    (Just n, Just p) -> put letterV $ n ++ " bought " ++ p

data Client
  = GovOrg
      { clientName :: String
      }
  | Company
      { clientName :: String
      , person :: Person
      , duty :: String
      }
  | Individual
      { person :: Person
      }
  deriving (Show, Eq, Ord)

data ClientKind
  = KindGovOrg
  | KindCompany
  | KindIndividual
  deriving (Show, Eq, Ord)

data Person =
  Person
    { firstName :: String
    , lastName :: String
    , gender :: Gender
    }
  deriving (Show, Eq, Ord)

data Gender
  = Male
  | Female
  | UnknownGender
  deriving (Show, Eq, Ord)

data Product' =
  Product'
    { productId :: Integer
    , productType :: ProductType
    }
  deriving (Show, Eq, Ord)

data ProductType
  = TimeMachine
  | TravelGuide
  | Tool
  | Trip
  deriving (Show, Eq, Ord)

data Purchase =
  Purchase
    { client :: Client
    , products :: [Product']
    }
  deriving (Show, Eq, Ord)

data PurchaseInfo
  = InfoClientKind ClientKind
  | InfoClientDuty String
  | InfoClientGender Gender
  | InfoPurchasedProduct Integer
  | InfoPurchasedProductType ProductType
  deriving (Show, Eq, Ord)

newtype Transaction =
  Transaction (Set PurchaseInfo)
  deriving (Eq, Ord)

newtype FrequentSet =
  FrequentSet (Set PurchaseInfo)
  deriving (Eq, Ord)

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      f (Transaction tElts) = sElts `S.isSubsetOf` tElts
      supp = length (filter f trans)
   in fromIntegral supp / fromIntegral total

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList
        {- HLINT ignore generateL1 -}

instance NFData FrequentSet where
  rnf = rnf

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupp transactions =
  runPar $ do
    let c1 =
          noDups $
          concatMap
            (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t)
            transactions
    l1NotFiltered <-
      parMap (\fs -> (fs, setSupport transactions fs > minSupp)) c1
    return $
      concatMap
        (\(fs, b) ->
           if b
             then [fs]
             else [])
        l1NotFiltered

generateNextLK ::
     Double
  -> [Transaction]
  -> (Int, [FrequentSet])
  -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLK _ _ (_, []) = Nothing
generateNextLK minSupp transactions (k, lk) =
  let ck1 =
        noDups $
        [ FrequentSet $ a `S.union` b
        | FrequentSet a <- lk
        , FrequentSet b <- lk
        , S.size (a `S.intersection` b) == k - 1
        ]
      lk1 = runPar $ filterLk minSupp transactions ck1
   in Just (lk1, (k + 1, lk1))

filterLk :: Double -> [Transaction] -> [FrequentSet] -> Par [FrequentSet]
filterLk minSupp transactions ck =
  let lengthCk = length ck
   in if lengthCk <= 5
        then return $ filter (\fs -> setSupport transactions fs > minSupp) ck
        else let (l, r) = splitAt (lengthCk `div` 2) ck
              in do lVar <- spawn $ filterLk minSupp transactions l
                    lFiltered <- get lVar
                    rVar <- spawn $ filterLk minSupp transactions r
                    rFiltered <- get rVar
                    return $ lFiltered ++ rFiltered

--
-- Ex 8-1
--
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

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

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

kMeans ::
     (Ord v, Vector v, Vectorizable e v, NFData v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int -- number of centroids
  -> [e] -- the information
  -> Double -- threshold
  -> ([v], Int) -- final centroids with no of iterations
kMeans i k points threshold = kMeans' (i k points) points threshold 1

kMeans' ::
     (Ord v, Vector v, Vectorizable e v, NFData v)
  => [v]
  -> [e]
  -> Double
  -> Int
  -> ([v], Int)
kMeans' centroids points threshold depth =
  runPar $ do
    let assignments = clusterAssignmentPhase centroids points
        oldNewCentroids = newCentroidPhase assignments
    newCentroids <- parMap snd oldNewCentroids
    if shouldStop oldNewCentroids threshold
      then return (newCentroids, depth)
      else return $ kMeans' newCentroids points threshold (depth + 1)

-- test kmeans
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v
