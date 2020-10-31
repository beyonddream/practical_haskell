{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
  EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, GADTs,
  GeneralizedNewtypeDeriving, OverloadedStrings, TypeApplications,
  OverloadedStrings #-}

module Chapter11.DBEx where

import Chapter11.Gender
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Int
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
        Country
            name String
            canWeSend Bool default=True
            UniqueCountryName name
            deriving Show
        Client
            firstName String
            lastName String
            address String
            country CountryId
            age Int Maybe
            gender Gender Maybe
            UniqueClient firstName lastName address country
            deriving Show
        Product
            name String
            description String
            price Double
            stock Int64
            UniqueProductName name
            deriving Show
        Purchase
            client ClientId
            product ProductId
            number Int
            amount Double
            deriving Show
        |]
