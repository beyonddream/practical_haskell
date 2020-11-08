{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Util
import Web.Spock.Digestive

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

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock cfg app)
        {- HLINT ignore app -}

app :: SpockM () () () ()
app = do
  get "about" $
    html $
    mconcat
      ["<html><body>", " <h1>Hello Practical Haskell!</h1>", "</body></html>"]
  hookAnyAll $ \_route -> do
    setStatus notFound404
    html "<h1>Not found :(</h1>"
