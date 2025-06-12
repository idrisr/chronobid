import Data.Time (UTCTime (..), secondsToDiffTime)
import Data.UUID (nil)
import Fmt
import Models.Order
import Models.OrderBook
import Models.Trade
import OrderMatcher.Matcher
import Test.Tasty
import Test.Tasty.HUnit
import TestSampleFiles

main :: IO ()
main = do
    f <- fileTests
    defaultMain $ testGroup "more" [tests, f]

tests :: TestTree
tests =
    testGroup
        "Matcher Tests"
        [ testCase "fully matches buy" $
            let book = OrderBook [] [mkOrder Sell Limit (Price 100) (Quantity 5)]
                incoming = mkOrder Buy Limit (Price 100) (Quantity 5)
                (trades, OrderBook bids asks) = matchOrder book incoming
             in do
                    assertEqual "trades match" trades [Trade nil nil 100 5]
                    assertEqual "empty bids" bids []
                    assertEqual "empty asks" asks []
        , testCase "partial fill buy" $
            let book = OrderBook [] [mkOrder Sell Limit (Price 100) (Quantity 3)]
                incoming = mkOrder Buy Limit (Price 100) (Quantity 5)
                (trades, OrderBook bids asks) = matchOrder book incoming
             in do
                    trades @?= [Trade nil nil 100 3]
                    bids @?= [incoming{quantity = 2}]
                    asks @?= []
        , testCase "no match" $
            let book = OrderBook [] [mkOrder Sell Limit (Price 105) (Quantity 5)]
                incoming = mkOrder Buy Limit (Price 100) (Quantity 5)
                (trades, OrderBook bids asks) = matchOrder book incoming
             in do
                    assertEqual "trades should be empty" [] trades
                    assertEqual "incoming buy should be added to bids" [incoming] bids
                    assertEqual "asks should be unchanged" [mkOrder Sell Limit (Price 105) (Quantity 5)] asks
        ]

-- helpers
t :: UTCTime
t = UTCTime (toEnum 0) (secondsToDiffTime 0)

newtype Price = Price Double
    deriving (Eq, Ord, Show)

newtype Quantity = Quantity Int
    deriving (Eq, Ord, Show)

mkOrder :: Side -> OrderType -> Price -> Quantity -> Order
mkOrder s ot (Price p) (Quantity q) =
    Order
        { orderId = nil
        , timestamp = t
        , side = s
        , orderType = ot
        , price = Just p
        , quantity = q
        , userId = nil
        }
