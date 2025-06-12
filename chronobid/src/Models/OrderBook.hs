{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.OrderBook where

import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Models.Order
import Servant.Docs hiding (path)

data OrderBook = OrderBook
    { bids :: [Order] -- buy orders, usually sorted descending by price
    , asks :: [Order] -- sell orders, usually sorted ascending by price
    }
    deriving (Eq, Show, Generic, ToJSON)

instance ToSample OrderBook where
    toSamples _ =
        singleSample $
            OrderBook [] []

loadOrders :: FilePath -> IO [Order]
loadOrders path = do
    csv <- BL.readFile path
    case decodeByName csv of
        Left err -> error $ "CSV parse error: " ++ err
        Right (_, vec) -> pure $ V.toList vec

loadOrderBook :: FilePath -> IO OrderBook
loadOrderBook path = do
    orders <- loadOrders path
    let bids_ = filter ((== Buy) . side) orders
        asks_ = filter ((== Sell) . side) orders
    pure $ OrderBook bids_ asks_
