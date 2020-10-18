{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Conduit
import System.Environment
import System.IO
import System.Random

main :: IO ()
main = do
  h <- openFile "./file_ex" ReadMode
  s <- hGetContents h
  hClose h
  print s

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
  deriving (Show, Read)

data Person =
  Person
    { firstName :: String
    , lastName :: String
    }
  deriving (Show, Ord, Read)

instance Eq Person where
  Person firstNameL lastNameL == Person firstNameR lastNameR =
    firstNameL == firstNameR && lastNameL == lastNameR

{- HLINT ignore processClients -}
processClients :: IO ()
processClients = do
  (inFile:outFile:_) <- getArgs
  bracket
    (openFile inFile ReadMode)
    hClose
    (\inHandle ->
       bracket
         (openFile (outFile ++ "_gov") WriteMode)
         hClose
         (\outFileGovHandle ->
            bracket
              (openFile (outFile ++ "_comp") WriteMode)
              hClose
              (\outFileCompHandle ->
                 bracket
                   (openFile (outFile ++ "_indv") WriteMode)
                   hClose
                   (\outFileIndvHandle ->
                      loop
                        inHandle
                        outFileGovHandle
                        outFileCompHandle
                        outFileIndvHandle))))
  where
    loop inHandle outFileGovHandle outFileCompHandle outFileIndvHandle = do
      isEof <- hIsEOF inHandle
      unless isEof $ do
        (client :: Client Integer) <- fmap read $ hGetLine inHandle
        case client of
          GovOrg {..} -> hPutStrLn outFileGovHandle (show client)
          Company {..} -> hPutStrLn outFileCompHandle (show client)
          Individual {..} -> hPutStrLn outFileIndvHandle (show client)
        loop inHandle outFileGovHandle outFileCompHandle outFileIndvHandle

readWriteEx :: IO ()
readWriteEx = do
  (inFile:outFile:_) <- getArgs
  inHandle <- openFile inFile ReadMode
  outHandle <- openFile outFile WriteMode
  loop inHandle outHandle
  hClose inHandle
  hClose outHandle
  where
    loop inHandle outHandle = do
      isEof <- hIsEOF inHandle
      unless isEof $ do
        client <- hGetLine inHandle
        (winner :: Bool) <- randomIO
        (year :: Int) <- randomRIO (0, 3000)
        hPrint outHandle $ show (client, winner, year)
        loop inHandle outHandle

playGame :: IO ()
playGame = do
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

print1 :: IO ()
print1 = do
  hSetBuffering stdout LineBuffering
  putStrLn "Where do you want to travel?"
  place <- getLine
  let year = length place * 10
  putStrLn $ "You should travel to year " ++ show year

people :: Monad m => ConduitT (Client i) Person m ()
people = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      case c of
        Company {person = p} -> yield p
        Individual {person = p} -> yield p
        _ -> return ()
      people
