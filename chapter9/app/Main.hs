{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Loops
import System.IO
import System.Random

main :: IO ()
main = do
  expected <- getRandomNumber 3 17
  putStrLn "Guess a number between 3 and 17"
  guessNumber expected 4

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber start end = randomRIO (start, end)

guessNumber :: Int -> Int -> IO ()
guessNumber expected maxGuesses = do
  actual <- fmap read getLine
  if actual == expected
    then putStrLn "Congrats! You have guessed correctly."
    else if maxGuesses /= 0
           then do
             putStrLn "Please try again:"
             guessNumber expected (maxGuesses - 1)
           else putStrLn $
                "Exhausted all tries. The number is " ++ show expected

print3 :: IO ()
print3 = do
  (initial :: Int) <- fmap read getLine
  jumps <-
    unfoldrM
      (\_ -> do
         next <- randomRIO (0, 3000)
         if next == initial
           then return Nothing
           else return $ Just (next, next))
      initial
  print $ take 10 jumps

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
