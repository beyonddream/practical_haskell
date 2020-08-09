module Chapter3.Section3.MoreModules where

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
