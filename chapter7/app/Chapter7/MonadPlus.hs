{-# LANGUAGE LambdaCase, MonadComprehensions #-}

module Chapter7.MonadPlus where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps year =
  nub $ do
    x <- [-1, 3, 5]
    y <- [-1, 3, 5]
    z <- [-1, 3, 5]
    return $ year + x + y + z

brokenJumps :: Int -> Int -> [Int]
brokenJumps year jumps =
  nub $ map (\x -> year + sum x) $ replicateM jumps [-1, 3, 5]

broken1 :: Integer -> [Integer]
broken1 n = [n - 1, n + 1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n + 2]

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f l = listToMaybe $ msum $ map (: []) $ mfilter f l

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

productsToPurchaseInfo :: [Product'] -> Set PurchaseInfo
productsToPurchaseInfo =
  foldr
    (\(Product' i t) pinfos ->
       S.insert (InfoPurchasedProduct i) $
       S.insert (InfoPurchasedProductType t) pinfos)
    S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo =
  \case
    GovOrg _ -> S.insert (InfoClientKind KindGovOrg) S.empty
    Company _ _ d ->
      S.insert (InfoClientKind KindCompany) $
      S.insert (InfoClientDuty d) S.empty
    Individual (Person _ _ g) ->
      S.insert (InfoClientKind KindIndividual) $
      S.insert (InfoClientGender g) S.empty

newtype FrequentSet =
  FrequentSet (Set PurchaseInfo)
  deriving (Eq, Ord)

data AssocRule =
  AssocRule (Set PurchaseInfo) (Set PurchaseInfo)
  deriving (Eq, Ord)

instance Show AssocRule where
  show (AssocRule a b) = show a ++ " => " ++ show b

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      f (Transaction tElts) = sElts `S.isSubsetOf` tElts
      supp = length (filter f trans)
   in fromIntegral supp / fromIntegral total

ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b) =
  setSupport trans (FrequentSet $ a `S.union` b) /
  setSupport trans (FrequentSet a)

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  noDups $ do
    Transaction t <- transactions
    e <- S.toList t
    let fs = FrequentSet $ S.singleton e
    guard $ setSupport transactions fs > minSupport
    return fs

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateNextLk ::
     Double
  -> [Transaction]
  -> (Int, [FrequentSet])
  -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
  let lk1 =
        noDups $ do
          FrequentSet a <- lk
          FrequentSet b <- lk
          guard $ S.size (a `S.intersection` b) == k - 1
          let fs = FrequentSet $ a `S.union` b
          guard $ setSupport transactions fs > minSupport
          return fs
   in Just (lk1, (k + 1, lk1))

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets = do
  FrequentSet fs <- sets
  subset@(_:_) <- powerset $ S.toList fs
  let ssubset = S.fromList subset
      rule = AssocRule ssubset (fs `S.difference` ssubset)
  guard $ ruleConfidence transactions rule > minConfidence
  return rule

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x :) (powerset xs)

apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions $
  concat $
  unfoldr
    (generateNextLk minSupport transactions)
    (1, generateL1 minSupport transactions)

paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- paths edges e_end end
        return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do
        (e_start, e_end) <- choices edges
        guard $ e_start == start
        subpath <- pathsL edges e_end end
        return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

choices :: [a] -> Logic a
choices = msum . map return

pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
  let e_paths =
        choices edges >>- \(e_start, e_end) ->
          guard (e_start == start) >>
          pathsLFair edges e_end end >>- \subpath -> return $ start : subpath
   in if start == end
        then return [end] `interleave` e_paths
        else e_paths

pathsL' :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL' edges start end =
  let e_paths =
        choices edges >>= \(e_start, e_end) ->
          guard (e_start == start) >> pathsL' edges e_end end >>= \subpath ->
            return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))

logInformation' :: [String] -> Writer String ()
logInformation' infos = forM_ infos $ \s -> tell (s ++ "\n")

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f * x)) 1 [1 .. n]

{- HLINT ignore powerset' -}
powerset' :: [a] -> [[a]]
powerset' = filterM (\_ -> [False, True])

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = execWriterT (pathsWriter' edges start end)

pathsWriter' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriter' edges start end =
  let e_paths = do
        (e_start, e_end) <- lift edges
        guard $ e_start == start
        tell [start]
        pathsWriter' edges e_end end
   in if start == end
        then tell [start] `mplus` e_paths
        else e_paths
