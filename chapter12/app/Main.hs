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
  get "clients" $ do
    (clients :: [Db.Entity Client]) <-
      runQuery $ \conn -> flip Db.runSqlPersistM conn $ Db.selectList [] []
    html $
      toStrict $
      renderHtml
        [shamlet|
                <html>
                    <body>
                        <h1>Clients
                        <table>
                            <tr>
                                <th>Name
                                <th>Address
                            $forall Db.Entity _ c <- clients
                                <tr>
                                    <td>#{clientFirstName c} #{clientLastName c}
                                    <td>#{clientAddress c}
            |]
  get ("client" <//> var) $ \(clientId :: Integer) -> do
    client <-
      runQuery $ \conn ->
        flip Db.runSqlPersistM conn $
        Db.get $ ClientKey (Db.SqlBackendKey $ fromIntegral clientId)
    case client of
      Just (Client {..}) ->
        html $
        toStrict $
        renderHtml
          [shamlet|
        <html>
            <head>
                <title>Client Info
                <body>
                    <h1>#{clientFirstName} #{clientLastName}
                    <p id=addr>#{clientAddress}
        |]
      Nothing -> do
        setStatus notFound404
        html "<h1>Client Not Found :(</h1>"
  get ("json" <//> "client" <//> var) $ \(clientId :: Integer) -> do
    client <-
      runQuery $ \conn ->
        flip Db.runSqlPersistM conn $
        Db.get $ ClientKey (Db.SqlBackendKey $ fromIntegral clientId)
    case client of
      Just c -> json c
      Nothing -> setStatus notFound404
  get "products" $ do
    (products :: [Db.Entity Product]) <-
      runQuery $ \conn -> flip Db.runSqlPersistM conn $ Db.selectList [] []
    html $
      toStrict $
      renderHtml
        [shamlet|
                <html>
                    <body>
                        <h1>Products
                        <table>
                            <tr>
                                <th>Name
                                <th>Description
                            $forall Db.Entity _ p <- products
                                <tr>
                                    <td>#{productName p}
                                    <td>#{productDescription p}
            |]
  get ("product" <//> var) $ \(productId :: Integer) -> do
    product <-
      runQuery $ \conn ->
        flip Db.runSqlPersistM conn $
        Db.get $ ProductKey (Db.SqlBackendKey $ fromIntegral productId)
    case product of
      Just (Product {..}) ->
        html $
        toStrict $
        renderHtml
          [shamlet|
        <html>
            <head>
                <title>Time Machine Store
                <body>
                    <h1>#{productName}
                    <p id=descr>#{productDescription}
        |]
      Nothing -> do
        setStatus notFound404
        html "<h1>Not Found :(</h1>"
  get ("json" <//> "product" <//> var) $ \(productId :: Integer) -> do
    product <-
      runQuery $ \conn ->
        flip Db.runSqlPersistM conn $
        Db.get $ ProductKey (Db.SqlBackendKey $ fromIntegral productId)
    case product of
      Just p -> json p
      Nothing -> setStatus notFound404
