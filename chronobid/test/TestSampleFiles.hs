module TestSampleFiles where

import qualified Data.Vector as V
import Models.OrderBook (loadOrders)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

fileParse :: FilePath -> TestTree
fileParse f = testCase f $ do
    _ <- loadOrders f -- will throw on failure
    pure () -- success = file parsed

fileTests :: IO TestTree
fileTests = do
    files <- listDirectory "data"
    absFiles <- mapM makeAbsolute $ ("data" </>) <$> files
    pure $ testGroup "CSV file parsing" $ fileParse <$> absFiles
