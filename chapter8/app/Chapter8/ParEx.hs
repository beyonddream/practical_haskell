module Chapter8.ParEx where

import Control.DeepSeq
import Control.Monad.Par
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
