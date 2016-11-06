{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Coin
import           Database.Redis                       (ConnectInfo (..),
                                                       connect,
                                                       defaultConnectInfo)
import           Network                              (PortID (PortNumber))

import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty                           (body, get, json,
                                                       middleware, param, post,
                                                       rescue, scottyOpts,
                                                       settings)

import           Control.Monad.IO.Class               (liftIO)
import           Data.Default.Class                   (def)
import qualified Data.Text.Lazy                       as T

import           Data.Aeson                           (decode, object, (.=))

import           Options.Applicative

data Options = Options { getHost      :: String
                       , getPort      :: Int
                       , getRedisHost :: String
                       , getRedisPort :: Int }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Coin server host."
                                <> value "127.0.0.1")

                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> help "Coin server port."
                                  <> value 3000)

                 <*> strOption (long "redis_host"
                                <> metavar "HOST"
                                <> help "Redis server host."
                                <> value "127.0.0.1" )

                 <*> option auto (long "redis_port"
                                  <> metavar "PORT"
                                  <> help "Redis server port."
                                  <> value 6379)

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Patent server"
     <> header "patent - Patent server" )

program :: Options -> IO ()
program opts = do
  conn <- connect $ defaultConnectInfo {
    connectHost = getRedisHost opts,
    connectPort = PortNumber . fromIntegral $ getRedisPort opts
  }

  let opts' = def { settings = setPort (getPort opts) $ setHost (Host $ getHost opts) (settings def) }
  scottyOpts opts' $ do
    middleware logStdoutDev
    get "/api/coins/:name/score/" $ do
      name <- param "name"
      score <- liftIO $ getScore conn name
      json $ object [ "score" .= score ]

    get "/api/coins/:name/" $ do
      name <- param "name"
      from <- param "from"  `rescue` (\_ -> return 0)
      size <- param "size" `rescue` (\_ -> return 10)
      ret <- liftIO $ getCoins conn name from size

      json $ object ["total" .= fst ret, "from" .= from, "size" .= size, "coins" .= snd ret]

    post "/api/coins/:name/" $ do
      name <- param "name"
      b <- body
      case (decode b :: Maybe Coin) of
        Just coin -> do
          score <- liftIO $ saveCoin conn name coin
          json $ object [ "score" .= score ]
        Nothing -> json $ object [ "err" .= T.pack "Coin format error" ]
