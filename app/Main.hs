{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Control.Monad.Reader                 (lift)
import           Data.Aeson                           (Value, decode, object,
                                                       (.=))
import qualified Data.ByteString.Lazy                 as LB (empty)
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.HTTP.Types                   (status204, status400)
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (ActionT, ScottyT, body,
                                                       get, json, middleware,
                                                       param, post, put, raw,
                                                       rescue, scottyOptsT,
                                                       settings, status)

import           Coin
import           Control.Monad                        (when)
import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl)

import           Data.Text.Lazy                       as LT (pack)

import qualified Coin.Config                          as C
import qualified Data.Yaml                            as Y

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getConfigFile  :: String
                       , getHost        :: String
                       , getPort        :: Int
                       , getTablePrefix :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Coin micro server config file."
                                <> value "config.yaml")
                 <*> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Coin micro server hostname."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT"
                                <> help "Coin micro server port."
                                <> value 3000)
                 <*> strOption (long "table_prefix"
                                <> metavar "TABLE_PREFIX"
                                <> help "table prefix."
                                <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Coin micro server"
     <> header "coin - Coin micro server" )

program :: Options -> IO ()
program opts = do
  (Just conf) <- Y.decodeFile (getConfigFile opts) :: IO (Maybe C.Config)

  let serverHost   = getHost opts
      serverPort   = getPort opts

      mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

      tablePrefix  = getTablePrefix opts

  mySQLPool <- C.genMySQLPool mysqlConfig

  let state = initCoinState mysqlThreads

  let userEnv = UserEnv { mySQLPool = mySQLPool, tablePrefix = tablePrefix }

  let opts = def { settings = setPort serverPort
                            $ setHost (Host serverHost) (settings def) }

  _ <- runIO userEnv state createTable
  scottyOptsT opts (runIO userEnv state) application
  where
        runIO :: UserEnv -> StateStore -> CoinM b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: ScottyM ()
application = do
  middleware logStdout

  get  "/api/coins/:name/score/" $ getScoreHandler
  get  "/api/coins/:name/info/"  $ getInfoHandler
  put  "/api/coins/:name/info/"  $ setInfoHandler
  get  "/api/coins/:name/"       $ getCoinListHandler
  post "/api/coins/:name/"       $ saveCoinHandler

getScoreHandler :: ActionM ()
getScoreHandler = do
  name <- param "name"
  score <- lift $ getScore name
  json $ object [ "score" .= score ]

getInfoHandler :: ActionM ()
getInfoHandler = do
  name  <- param "name"
  info  <- lift $ getInfo name
  score <- lift $ getScore name
  json $ object [ "score" .= score, "info" .= info, "name" .= name ]

setInfoHandler :: ActionM ()
setInfoHandler = do
  name  <- param "name"
  wb <- body
  case (decode wb :: Maybe Value) of
    Nothing -> status status400 >> raw LB.empty
    Just v -> do
      lift $ setInfo name v
      status status204
      raw LB.empty

getCoinListHandler :: ActionM ()
getCoinListHandler = do
  name <- param "name"
  from <- param "from"  `rescue` (\_ -> return 0)
  size <- param "size" `rescue` (\_ -> return 10)

  ret <- lift $ getCoins name from size
  total <- lift $ countCoin name
  json $ object ["total" .= total, "from" .= from, "size" .= size, "coins" .= ret]

saveCoinHandler :: ActionM ()
saveCoinHandler = do
  name  <- param "name"
  score <- param "score"
  desc  <- param "desc" `rescue` (\_ -> return "")
  tp    <- param "type"
  ct    <- param "created_at" `rescue` (\_ -> return 0)

  case readType tp of
    Just tp' -> do
      ret <- lift $ saveCoin name (zeroCoin { getCoinScore = score
                                            , getCoinType = tp'
                                            , getCoinDesc = desc
                                            , getCoinCreatedAt = ct
                                            })


      json $ object [ "score" .= ret ]
    Nothing -> json $ object [ "err" .= pack "Invalid type" ]

  where readType :: String -> Maybe CoinType
        readType "Incr" = Just Incr
        readType "Decr" = Just Decr
        readType "incr" = Just Incr
        readType "decr" = Just Decr
        readType "INCR" = Just Incr
        readType "DECR" = Just Decr
        readType _      = Nothing
