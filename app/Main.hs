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
import           Network.Wai.Middleware.RequestLogger (logStdout)
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
                       , getRedisDb   :: Int
                       , getPrefix    :: String
                       }

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

                 <*> strOption (long "prefix"
                                <> metavar "PREFIX"
                                <> help "Redis prefix key."
                                <> value "")


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
  scottyOptsT opts' (runRedis conn) $ application (fixPrefix $ getPrefix opts)

fixPrefix :: String -> String -> String
fixPrefix [] ys                  = ys
fixPrefix xs ys | last xs == ':' = xs ++ ys
                | otherwise      = xs ++ [':'] ++ ys


application :: (String -> String) -> ScottyM ()
application fix = do
  middleware logStdout
  get "/api/coins/:name/score/" $ getScoreHandler fix
  get "/api/coins/:name/" $ getCoinListHandler fix
  post "/api/coins/:name/" $ saveCoinHandler fix

getScoreHandler :: (String -> String) -> ActionM ()
getScoreHandler fix = do
  name <- param "name"
  score <- lift $ getScore (fix name)
  json $ object [ "score" .= score ]

getCoinListHandler :: (String -> String) -> ActionM ()
getCoinListHandler fix = do
  name <- param "name"
  from <- param "from"  `rescue` (\_ -> return 0)
  size <- param "size" `rescue` (\_ -> return 10)

  ret <- lift $ getCoins (fix name) from size
  json $ object ["total" .= fst ret, "from" .= from, "size" .= size, "coins" .= snd ret]

saveCoinHandler :: (String -> String) -> ActionM ()
saveCoinHandler fix = do
  name  <- param "name"
  score <- param "score"
  desc  <- param "desc" `rescue` (\_ -> return "")
  tp    <- param "type"
  ct    <- param "created_at" `rescue` (\_ -> return 0)

  case readType tp of
    Just tp' -> do
      ret <- lift $ saveCoin (fix name) (zeroCoin { getCoinScore = score
                                                  , getCoinType = tp'
                                                  , getCoinDesc = desc
                                                  , getCoinCreatedAt = ct
                                                  })


      json $ object [ "score" .= ret ]
    Nothing -> json $ object [ "err" .= LT.pack "Invalid type" ]

  where readType :: String -> Maybe CoinType
        readType "Incr" = Just Incr
        readType "Decr" = Just Decr
        readType "incr" = Just Incr
        readType "decr" = Just Decr
        readType "INCR" = Just Incr
        readType "DECR" = Just Decr
        readType _      = Nothing
