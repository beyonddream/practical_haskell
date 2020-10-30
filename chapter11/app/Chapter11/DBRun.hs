{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Chapter11.DBRun where

import Chapter11.DBEx
import Chapter11.Gender
import Control.Monad.Logger
import Control.Monad.Trans
import Database.Persist.Sqlite
import Database.Persist.TH

exampleMigration =
  runSqlite @IO @SqlBackend "example.db" $ runMigration migrateAll

exampleRun =
  runSqlite @IO @SqlBackend "example.db" $ do
    carri <- insert $ Country "Carribean" False
    _product1 <- insert $ Product "product1" "A new product1" 123.5 10
    _client1 <-
      insert $ Client "Ak" "uu" "Home Town, 1" carri (Just 3) (Just Male)
    return ()
