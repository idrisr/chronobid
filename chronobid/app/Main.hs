module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import Models.Order

loadOrders :: FilePath -> IO [Order]
loadOrders path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> error $ "CSV parse error: " ++ err
        Right (_, vec) -> pure $ V.toList vec

main :: IO ()
main = do
    orders <- loadOrders "data/orders.csv"
    mapM_ print orders
