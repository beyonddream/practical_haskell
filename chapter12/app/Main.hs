{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
  MultiParamTypeClasses, FlexibleContexts, GADTs, OverloadedStrings,
  GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications,
  FlexibleInstances #-}

module Main where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Database.Persist.Sqlite as Db
import Database.Persist.TH
import Network.HTTP.Types
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Hamlet
import Web.Spock
import Web.Spock.Config

import Chapter12.Database
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Util
import Web.Spock.Digestive

main :: IO ()
main = do
  Db.runSqlite "example.db" $ Db.runMigration migrateAll
  runNoLoggingT $
    Db.withSqlitePool "example.db" 10 $ \pool ->
      liftIO $ do
        cfg <- defaultSpockCfg () (PCPool pool) ()
        runSpock 3000 (spock cfg app)
        {- HLINT ignore app -}

app :: SpockM Db.SqlBackend () () ()
app = do
  get ("product" <//> var) $ \(productId :: Integer) -> do
    product <-
      runQuery $ \conn ->
        flip Db.runSqlPersistM conn $
        Db.get $ ProductKey (Db.SqlBackendKey $ fromIntegral productId)
    case product of
      Just (Product {..}) ->
        html $
        mconcat
          [ "<html><body>"
          , "<h1>"
          , T.pack productName
          , "</h1>"
          , "<p>"
          , T.pack productDescription
          , "</p>"
          , "</body></html>"
          ]
      Nothing -> do
        setStatus notFound404
        html "<h1>Not Found :(</h1>"
