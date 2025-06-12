import API.OrderBookAPI
import Data.Proxy
import Servant.Docs

main :: IO ()
main = writeFile "API.md" (markdown $ docs (Proxy :: Proxy APIRoutes))
