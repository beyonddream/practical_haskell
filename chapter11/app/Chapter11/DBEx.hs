{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
  EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, GADTs,
  GeneralizedNewtypeDeriving, OverloadedStrings, TypeApplications,
  OverloadedStrings #-}

module Chapter11.DBEx where

import Chapter11.Gender
import Control.Monad.Logger
import Control.Monad.Trans
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
        |]
