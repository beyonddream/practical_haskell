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
    spain <- insert $ Country "Spain" False
    _client1 <-
      insert $
      Client "Alejandro" "Serrano" "Home Town, 1" spain (Just 30) (Just Male)
    return ()
