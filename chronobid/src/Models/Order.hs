{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Order where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Csv
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Fmt
import GHC.Generics (Generic)
import Servant.Docs

instance Buildable Order where
    build o =
        nameF "Order" $
            blockListF
                [ "id" +| orderId o |+ ""
                , "side" +| side o |+ ""
                , "type" +| orderType o |+ ""
                , "price" +| price o |+ ""
                , ("qty" +| quantity o |+ "") :: String
                ]

data Side = Buy | Sell
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Buildable UUID where
    build = build . show

instance Buildable Side where
    build = build . show

instance Buildable OrderType where
    build = build . show

data OrderType = Limit | Market
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Order = Order
    { orderId :: UUID -- unique ID
    , timestamp :: UTCTime -- time of order placement
    , side :: Side -- buy or sell
    , orderType :: OrderType -- limit or market
    , price :: Maybe Double -- price (Nothing for market)
    , quantity :: Int -- quantity remaining
    , userId :: UUID -- user placing the order
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToSample Order where
    toSamples _ =
        singleSample $
            Order
                (read "00000000-0000-0000-0000-000000000001") -- UUID
                (read "2025-06-11 18:00:00 UTC") -- UTCTime
                Buy
                Limit
                (Just 100.0)
                3 -- Int quantity
                (read "00000000-0000-0000-0000-000000000002") -- userId UUID

newtype UTime = UTime {unUTime :: UTCTime}
    deriving (Show)

instance FromField UTime where
    parseField bs =
        let str = BS.unpack bs
         in case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" str of
                Just t -> pure (UTime t)
                Nothing -> fail $ "Invalid timestamp: " ++ str

instance FromNamedRecord Order where
    parseNamedRecord m = do
        orderId_ <- parseUUID "uuid" m
        UTime timestamp_ <- m .: "timestamp"
        side_ <- m .: "side"
        orderType_ <- m .: "orderType"
        price_ <- m .: "price"
        quantity_ <- m .: "quantity"
        userId_ <- parseUUID "userUuid" m
        pure $ Order orderId_ timestamp_ side_ orderType_ price_ quantity_ userId_

instance FromField Side where
    parseField "Buy" = pure Buy
    parseField "Sell" = pure Sell
    parseField other = fail $ "Unknown order type: " ++ show other

instance FromField OrderType where
    parseField "Limit" = pure Limit
    parseField "Market" = pure Market
    parseField other = fail $ "Unknown order type: " ++ show other

parseUUID :: BS.ByteString -> NamedRecord -> Parser UUID
parseUUID s m = do
    raw <- m .: s -- :: ByteString
    maybe (fail "Invalid UUID") pure (UUID.fromASCIIBytes raw)

-- instance ToSample OrderResponse where
-- toSamples _ =
-- singleSample $
-- OrderResponse
-- (Order "uuid1" "2025-06-11T18:00Z" Buy Market Nothing 3.0 "user123")
-- [Trade 100.0 3.0 "user123" "user456" "2025-06-11T18:00Z"]
