{-# LANGUAGE LambdaCase #-}

module Chapter7.MonadPlus where

import Control.Monad
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

data Product =
  Product
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
    , products :: [Product]
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

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo =
  foldr
    (\(Product i t) pinfos ->
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
