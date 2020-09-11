{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.StateLenses where

import Control.Monad.State
import Data.Char
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
