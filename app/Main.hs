{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HFlags
import Database.Redis (connect, defaultConnectInfo, ConnectInfo(..))
import Network (PortID(PortNumber))
import Lib

import Web.Scotty
import Network.Wai.Handler.Warp (setPort, setHost)
import Network.Wai.Middleware.RequestLogger
import Data.Streaming.Network.Internal (HostPreference(Host))

import Control.Monad.IO.Class (liftIO)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Default.Class (def)

defineFlag "h:host" ("127.0.0.1"::String) "Coin server host."
defineFlag "p:port" (3000::Int) "Coin server port."
defineFlag "redis_host" ("127.0.0.1"::String) "Redis server host."
defineFlag "redis_port" (6379::Int) "Redis server port."
return []

main :: IO ()
main = do
  s <- $initHFlags "Coin v0.1"
  conn <- connect $ defaultConnectInfo {
    connectHost = flags_redis_host,
    connectPort = PortNumber $ fromIntegral flags_redis_port
  }

  let opts = def { settings = setPort flags_port $ setHost (Host flags_host) (settings def) }
  scottyOpts opts $ do
    middleware logStdoutDev
    get "/api/:name/coins/score/" $ do
      name <- param "name"
      score <- liftIO $ getScore conn name
      text $ packScore score

    get "/api/:name/coins/" $ do
      name <- param "name"
      from <- param "from"
      size <- param "size"
      ret <- liftIO $ getCoins conn name from size
      case ret of
        Just (total, v) -> text . mconcat $ [
          "Total ",
          T.pack $ show from,
          " ",
          T.pack $ show size,
          " ",
          T.pack $ show total, "\n" ]  ++ map toText v
        Nothing -> text ""

    post "/api/:name/coins/" $ do
      name <- param "name"
      b <- body
      let coin = read . T.unpack $ decodeUtf8 b :: Coin Integer Integer String
      score <- liftIO $ saveCoin conn name coin
      text $ packScore score

packScore :: Maybe Integer -> T.Text
packScore (Just v) = T.pack $ show v
packScore Nothing = "0"

toText :: Maybe (Coin Integer Integer String) -> T.Text
toText (Just v) = T.pack (show v ++ "\n")
toText Nothing = ""
