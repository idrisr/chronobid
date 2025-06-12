{-# LANGUAGE OverloadedStrings #-}

module Models.Trade where

import Data.UUID (UUID)
import Fmt
import Servant.Docs

data Trade = Trade
    { takerId :: UUID
    , makerId :: UUID
    , tradePrice :: Double
    , tradeQuantity :: Int
    }
    deriving (Show, Eq)

instance ToSample Trade where
    toSamples _ =
        singleSample $
            Trade
                (read "00000000-0000-0000-0000-000000000001") -- takerId :: UUID
                (read "00000000-0000-0000-0000-000000000002") -- makerId :: UUID
                100.0 -- tradePrice :: Double
                3 -- tradeQuantity :: Int

instance Buildable UUID where build = build . show

instance Buildable Trade where
    build t =
        nameF "Trade" $
            blockListF
                [ "taker: " +| takerId t |+ ""
                , "maker: " +| makerId t |+ ""
                , "price: " +| tradePrice t |+ ""
                , ("quantity: " +| tradeQuantity t |+ "") :: String
                ]
