module OrderBookServer where

import API.OrderBookAPI
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Text (unpack)
import Models.Order
import Models.OrderBook
import Network.Wai
import OrderMatcher.Matcher
import Servant hiding (respond)
import System.FilePath

orderBookServer :: OrderBook -> Server OrderBookAPI
orderBookServer = pure

server :: FilePath -> TVar OrderBook -> Server FullAPI
server fp state =
    getBook
        :<|> postOrder
        :<|> docsHandler
        -- :<|> serveDirectoryWebApp fp
        :<|> serveStatic
  where
    getBook = liftIO $ readTVarIO state

    postOrder :: Order -> Handler NoContent
    postOrder o = liftIO . atomically $ do
        modifyTVar' state $ \book ->
            let (_, updatedBook) = matchOrder book o
             in updatedBook
        pure NoContent

    docsHandler :: Handler String
    docsHandler = liftIO . readFile $ "API.md"

    serveStatic :: Tagged Handler Application
    serveStatic =
        Tagged $ \req respond -> do
            let subpath = intercalate "/" $ map unpack $ pathInfo req
            putStrLn $ "🔍 static request: " ++ (fp </> subpath)
            unTagged (serveDirectoryFileServer fp) req respond
