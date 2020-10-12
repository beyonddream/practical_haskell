{-# LANGUAGE OverloadedStrings #-}

module Chapter8.AmqpEx where

import Control.Concurrent.STM
import Control.Exception -- needed later
import Network.AMQP.Worker

type Order = (String, Integer)

ordersQueue :: Queue Direct Order
ordersQueue =
  let testExchange = exchange "test"
   in queue testExchange "orders"

initialize :: IO Connection
initialize = do
  conn <- connect (fromURI "amqp://guest:guest@localhost:5672")
  initQueue conn ordersQueue
  return conn

frontend :: Connection -> IO ()
frontend conn = do
  publish conn ordersQueue ("product1", 10)
  putStrLn "Message Sent"

backend :: Connection -> IO ()
backend conn = do
  m <- newTVarIO 1000
  s <- newTVarIO [("a", 7)]
  putStrLn "Starting backend..."
  worker def conn ordersQueue onBackendError (onBackendMessage m s)

onBackendMessage ::
     TVar Integer -> TVar [(String, Integer)] -> Message Order -> IO ()
onBackendMessage _ _ Message {value = (product', price)} = do
  putStrLn $ "Received order " ++ show (product', price)
  putStrLn "paying..."

onBackendError :: WorkerException SomeException -> IO ()
onBackendError e = putStrLn $ "ERROR: " ++ show e
