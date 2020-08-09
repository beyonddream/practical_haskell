{-# LANGUAGE PatternSynonyms #-}

module Chapter3.Section3.MoreModules
  ( Range()
  , range
  --, RangeObs(..)
  --, r
  ) where

import Chapter3.Section1.HigherOrder (Client(GovOrg)) -- Client type and GovOrg constructor only

--import Data.List (permutations, subsequences)
import Data.List hiding (head, tail)

--import qualified Data.List (filter, permutations)
import qualified Data.List as L (filter, permutations)

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . permutations

permutationsStartingWith' :: Char -> String -> [String]
        {-permutationsStartingWith' letter =
  Data.List.filter (\l -> head l == letter) . Data.List.permutations-}
permutationsStartingWith' letter =
  L.filter (\l -> head l == letter) . L.permutations

-- smart constructor example
data Range =
  Range Integer Integer
  deriving (Show)

range :: Integer -> Integer -> Range
range a b =
  if a <= b
    then Range a b
    else error "a must be <= b"
        {-data RangeObs =
  R Integer Integer
  deriving (Show)

r :: Range -> RangeObs
r (Range a b) = R a b -}

prettyRange :: Range -> String
prettyRange rng =
  case rng of
    Range a b -> "[" ++ show a ++ "," ++ show b ++ "]"

pattern R :: Integer -> Integer -> Range

pattern R a b <- Range a b
  where R a b = range a b
