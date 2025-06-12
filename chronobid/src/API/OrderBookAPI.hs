{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.OrderBookAPI where

import Models.Order
import Models.OrderBook
import Servant.API

type OrderBookAPI = "orderbook" :> Get '[JSON] OrderBook
type OrderSubmitAPI = "order" :> ReqBody '[JSON] Order :> Post '[JSON] NoContent
type DocsAPI = "docs" :> Get '[PlainText] String

type APIRoutes = OrderBookAPI :<|> OrderSubmitAPI
type FullAPI =
    OrderBookAPI
        :<|> OrderSubmitAPI
        :<|> DocsAPI
        :<|> Raw
