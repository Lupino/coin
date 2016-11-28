{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Pool                            (createPool)
import           Database.MySQL.Simple                (ConnectInfo (..), close,
                                                       connect,
                                                       defaultConnectInfo)

import           Control.Monad.Reader                 (lift)
import           Data.Aeson                           (object, (.=))
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (ActionT, ScottyT, body,
                                                       get, json, middleware,
                                                       param, post, rescue,
                                                       scottyOptsT, settings)

import           Coin
import           Control.Monad                        (when)
import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl)

import           Data.Text.Lazy                       as LT (pack)
import           Data.Yaml.Config                     as Y (load, lookupDefault,
                                                            subconfig)
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
     <> header "dispatch-user - Coin micro server" )

program :: Options -> IO ()
program opts = do
  yamlConfig <- load $ getConfigFile opts
  mysqlConfig <- subconfig "mysql" yamlConfig
  let serverHost   = getHost opts
      serverPort   = getPort opts
      dbName       = Y.lookupDefault "db" "dispatch_user" mysqlConfig
      dbHost       = Y.lookupDefault "host" "127.0.0.1" mysqlConfig
      dbPort       = Y.lookupDefault "port" 3306 mysqlConfig
      dbUser       = Y.lookupDefault "user" "root" mysqlConfig
      dbPass       = Y.lookupDefault "pass" "" mysqlConfig
      numStripes   = Y.lookupDefault "numStripes" 1 mysqlConfig
      idleTime     = Y.lookupDefault "idleTime" 0.5 mysqlConfig
      maxResources = Y.lookupDefault "maxResources" 1 mysqlConfig
      numThreads   = Y.lookupDefault "numThreads" 1 mysqlConfig
      tablePrefix  = getTablePrefix opts

  let conn = connect defaultConnectInfo { connectDatabase = dbName
                                        , connectHost = dbHost
                                        , connectPort = dbPort
                                        , connectUser = dbUser
                                        , connectPassword = dbPass
                                        }

  pool <- createPool conn close numStripes idleTime maxResources

  let state = initCoinState numThreads

  let userEnv = UserEnv { mySQLPool = pool, tablePrefix = tablePrefix }

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
