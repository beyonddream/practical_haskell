{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
  MultiParamTypeClasses, FlexibleContexts, GADTs, OverloadedStrings,
  GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications,
  FlexibleInstances #-}

module Chapter12.Database where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Int
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Country json
  name String
  canWeSend Bool default=True
  UniqueCountryName name
  deriving Show
Client json
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int
  UniqueClient firstName lastName address country
  deriving Show
Product json
  name String
  description String
  price Double
  inStock Int
  deriving Show
Purchase json
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]
