{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Coin
import           Database.Redis                       (ConnectInfo (..),
                                                       Connection, Redis,
                                                       connect,
                                                       defaultConnectInfo,
                                                       runRedis)
import           Network                              (PortID (PortNumber))

import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty.Trans                     (ActionT, ScottyT, body,
                                                       get, json, middleware,
                                                       param, post, rescue,
                                                       scottyOptsT, settings)

import           Control.Monad.IO.Class               (liftIO)
import           Data.Default.Class                   (def)
import qualified Data.Text.Lazy                       as LT (Text, pack)

import           Data.Aeson                           (decode, object, (.=))

import           Control.Monad.Reader                 (lift)

import           Options.Applicative

data Options = Options { getHost      :: String
                       , getPort      :: Int
                       , getRedisHost :: String
                       , getRedisPort :: Int
                       , getRedisDb   :: Int }

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

                 <*> option auto (long "redis_db"
                                  <> metavar "DB"
                                  <> help "Redis server db."
                                  <> value 0)


type ActionM a = ActionT LT.Text Redis a
type ScottyM a = ScottyT LT.Text Redis a

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Coin server"
     <> header "coin - Coin server" )

program :: Options -> IO ()
program opts = do
  conn <- connect $ defaultConnectInfo {
    connectHost = getRedisHost opts,
    connectPort = PortNumber . fromIntegral $ getRedisPort opts,
    connectDatabase = fromIntegral $ getRedisDb opts
  }

  let opts' = def { settings = setPort (getPort opts) $ setHost (Host $ getHost opts) (settings def) }
  scottyOptsT opts' (runRedis conn) application


application :: ScottyM ()
application = do
  middleware logStdoutDev
  get "/api/coins/:name/score/" $ getScoreHandler
  get "/api/coins/:name/" $ getCoinListHandler
  post "/api/coins/:name/" $ saveCoinHandler

getScoreHandler :: ActionM ()
getScoreHandler = do
  name <- param "name"
  score <- lift $ getScore name
  json $ object [ "score" .= score ]

getCoinListHandler :: ActionM ()
getCoinListHandler = do
  name <- param "name"
  from <- param "from"  `rescue` (\_ -> return 0)
  size <- param "size" `rescue` (\_ -> return 10)

  ret <- lift $ getCoins name from size
  json $ object ["total" .= fst ret, "from" .= from, "size" .= size, "coins" .= snd ret]

saveCoinHandler :: ActionM ()
saveCoinHandler = do
  name  <- param "name"
  score <- param "score"
  desc  <- param "desc" `rescue` (\_ -> return "")
  tp    <- param "type"
  ct    <- param "created_at" `rescue` (\_ -> return 0)

  ret <- lift $ saveCoin name (zeroCoin { getCoinScore = score
                                        , getCoinType = read tp
                                        , getCoinDesc = desc
                                        , getCoinCreatedAt = ct
                                        })


  json $ object [ "score" .= ret ]
