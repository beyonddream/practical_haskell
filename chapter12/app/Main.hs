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
  get "new-product" $ do
    view <- getForm "product" productForm
    let view' = fmap H.toHtml view
    html $
      toStrict $
      renderHtml $
      H.html $ do
        H.head $ H.title "Time Machine Store"
        H.body $ productView view'
  post "new-product" $ do
    (view, product) <- runForm "product" productForm
    case product of
      Just p -> do
        ProductKey (Db.SqlBackendKey newId) <-
          runQuery $ \conn -> flip Db.runSqlPersistM conn $ Db.insert p
        redirect $ mconcat ["/product/", T.pack $ show newId]
      Nothing -> do
        let view' = fmap H.toHtml view
        html $
          toStrict $
          renderHtml $
          H.html $ do
            H.head $ H.title "Time Machine Store"
            H.body $ productView view'
  get "register" $ do
    view <- getForm "client" clientForm
    let view' = fmap H.toHtml view
    html $
      toStrict $
      renderHtml $
      H.html $ do
        H.head $ H.title "Register Client"
        H.body $ clientView view'
  post "register" $ do
    (view, client) <- runForm "client" clientForm
    case client of
      Just (Client' f l a c) -> do
        newCountryKey <-
          runQuery $ \conn ->
            flip Db.runSqlPersistM conn $ Db.insert (Country c True)
        ClientKey (Db.SqlBackendKey newId) <-
          runQuery $ \conn ->
            flip Db.runSqlPersistM conn $
            Db.insert (Client f l a newCountryKey 0)
        redirect $ mconcat ["/client/", T.pack $ show newId]
      Nothing -> do
        let view' = fmap H.toHtml view
        html $
          toStrict $
          renderHtml $
          H.html $ do
            H.head $ H.title "Register Client"
            H.body $ clientView view'

data Client' =
  Client' String String String String

clientForm :: Monad m => Form String m Client'
clientForm =
  Client' <$> "firstName" .: string Nothing <*> "lastName" .: string Nothing <*>
  "address" .: string Nothing <*>
  "country" .: choice countries Nothing
  where
    countries = [(x, show x) | x <- ["USA", "AUS", "JPN"]]

clientView :: View H.Html -> H.Html
clientView view = do
  form view "/register" $ do
    label "firstName" view "First Name:"
    inputText "firstName" view
    H.br
    label "lastName" view "Last Name:"
    inputText "lastName" view
    H.br
    inputTextArea Nothing Nothing "address" view
    H.br
    label "country" view "Country:"
    inputSelect "country" view
    H.br
    inputSubmit "Submit"

countryForm :: Monad m => Form String m Country
countryForm =
  Country <$> "name" .: string Nothing <*> "send" .: bool (Just True)

productForm :: Monad m => Form String m Product
productForm =
  Product <$> "name" .: string Nothing <*> "description" .: string Nothing <*>
  "price" .: validate isANumber (string Nothing) <*>
  "inStock" .: check "Must be >= 0" (>= 0) (validate isANumber (string Nothing))

isANumber :: (Num a, Read a) => String -> Result String a
isANumber = maybe (Error "Not a number") Success . readMaybe
        {- HLINT ignore productView -}

productView :: View H.Html -> H.Html
productView view = do
  form view "/new-product" $ do
    label "name" view "Name:"
    inputText "name" view
    H.br
    inputTextArea Nothing Nothing "description" view
    H.br
    label "price" view "Price:"
    inputText "price" view
    errorList "price" view
    label "inStock" view "# in Stock:"
    inputText "inStock" view
    errorList "inStock" view
    H.br
    inputSubmit "Submit"
