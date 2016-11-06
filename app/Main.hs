{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Coin
import           Database.Redis                       (ConnectInfo (..),
                                                       connect,
                                                       defaultConnectInfo)
import           HFlags
import           Network                              (PortID (PortNumber))

import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

import           Control.Monad.IO.Class               (liftIO)
import           Data.Default.Class                   (def)
import qualified Data.Text.Lazy                       as T
import           Data.Text.Lazy.Encoding              (decodeUtf8)

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
    get "/api/coins/:name/score/" $ do
      name <- param "name"
      score <- liftIO $ getScore conn name
      text $ packScore score

    get "/api/coins/:name/" $ do
      name <- param "name"
      from <- param "from"  `rescue` (\_ -> return 0)
      size <- param "size" `rescue` (\_ -> return 10)
      ret <- liftIO $ getCoins conn name from size
      text . mconcat $ [
        "Total ",
        T.pack $ show from,
        " ",
        T.pack $ show size,
        " ",
        T.pack $ show (fst ret), "\n" ] ++ map toText (snd ret)

    post "/api/coins/:name/" $ do
      name <- param "name"
      b <- body
      let coin = read . T.unpack $ decodeUtf8 b :: Coin
      score <- liftIO $ saveCoin conn name coin
      text $ packScore score

packScore :: Maybe Integer -> T.Text
packScore (Just v) = T.pack $ show v
packScore Nothing  = "0"

toText :: Maybe Coin -> T.Text
toText (Just v) = T.pack (show v ++ "\n")
toText Nothing  = ""
