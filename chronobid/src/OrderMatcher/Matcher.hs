module OrderMatcher.Matcher where

import Models.Order
import Models.OrderBook
import Models.Trade
import Prelude hiding (pred)

matchOrder :: OrderBook -> Order -> ([Trade], OrderBook)
matchOrder book o@Order{side = Buy} =
    let (trades, leftover, asks') = matchLoop (<=) matchSell [] o (asks book)
     in (trades, book{asks = asks', bids = maybe (bids book) (: bids book) leftover})
matchOrder book o@Order{side = Sell} =
    let (trades, leftover, bids') = matchLoop (>=) matchBuy [] o (bids book)
     in (trades, book{bids = bids', asks = maybe (asks book) (: asks book) leftover})

matchSell :: Order -> Bool
matchSell o = side o == Sell && orderType o == Limit

matchBuy :: Order -> Bool
matchBuy o = side o == Buy && orderType o == Limit

matchLoop ::
    (Double -> Double -> Bool) ->
    (Order -> Bool) ->
    [Trade] ->
    Order ->
    [Order] ->
    ([Trade], Maybe Order, [Order])
matchLoop priceOK pred acc o (x : xs)
    | pred x
    , Just px <- price x
    , Just po <- price o
    , priceOK px po =
        let q = min (quantity o) (quantity x)
            o' = o{quantity = quantity o - q}
            x' = x{quantity = quantity x - q}
            trade = Trade (userId o) (userId x) px q
            rest = if quantity x > q then x' : xs else xs
         in if quantity o <= q
                then (acc ++ [trade], Nothing, rest)
                else matchLoop priceOK pred (acc ++ [trade]) o' rest
matchLoop _ _ acc o rest = (acc, Just o, rest)
