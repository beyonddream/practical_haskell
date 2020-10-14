module Main where

import System.IO

main :: IO ()
main
    --print1
 = print2

print2 :: IO ()
print2 = do
  putStrLn "First name?"
  fName <- getLine
  putStrLn "Last name?"
  lName <- getLine
  putChar '>' >> putChar ' '
  print $ Person fName lName

data Person =
  Person
    { firstName :: String
    , lastName :: String
    }
  deriving (Show, Eq, Ord)

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

print1 :: IO ()
print1 = do
  hSetBuffering stdout LineBuffering
  putStrLn "Where do you want to travel?"
  place <- getLine
  let year = length place * 10
  putStrLn $ "You should travel to year " ++ show year
