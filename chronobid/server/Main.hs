{-# LANGUAGE DataKinds #-}

module Main where

import API.OrderBookAPI
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Models.Order
import Models.OrderBook
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import OrderBookServer
import Servant

api :: Proxy FullAPI
api = Proxy

initVar :: OrderBook -> STM (TVar OrderBook)
initVar = newTVar

orderBookFromCSV :: FilePath -> IO OrderBook
orderBookFromCSV path = do
    orders <- loadOrders path
    let bids_ = filter (\o -> side o == Buy) orders
        asks_ = filter (\o -> side o == Sell) orders
    pure $ OrderBook bids_ asks_

main :: IO ()
main = do
    let staticDir = "server/frontend"
    ob <- orderBookFromCSV "data/orders.csv"
    t <- atomically $ initVar ob
    run 8080 $ logStdout $ serve api (server staticDir t)
