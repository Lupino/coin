{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Coin.GraphQL                         (schema, schemaByUser)
import           Control.Monad.Reader                 (lift)
import           Data.Aeson                           (Value, decode, object,
                                                       (.=))
import qualified Data.ByteString.Lazy                 as LB (empty)
import           Data.Default.Class                   (def)
import           Data.GraphQL                         (graphql)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.HTTP.Types                   (status204)
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (body, get, json,
                                                       middleware, param, post,
                                                       put, raw, rescue,
                                                       scottyOptsT, settings,
                                                       status)

import           Coin
import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl, stateEmpty,
                                                       stateSet)

import           Yuntan.Types.ListResult              (ListResult (..))
import           Yuntan.Utils.Scotty                  (errBadRequest, ok,
                                                       okListResult)


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
program Options { getConfigFile = confFile
                , getHost = host
                , getPort = port
                , getTablePrefix = prefix
                } = do
  (Just conf) <- Y.decodeFile confFile :: IO (Maybe C.Config)

  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

  pool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initCoinState mysqlThreads) stateEmpty

  let userEnv = UserEnv { mySQLPool = pool, tablePrefix = prefix }

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

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

  get  "/api/coins/:name/score/" getScoreHandler
  get  "/api/coins/:name/info/"  getInfoHandler
  put  "/api/coins/:name/info/"  setInfoHandler
  get  "/api/coins/:name/"       getCoinListHandler
  post "/api/coins/:name/"       saveCoinHandler
  post "/api/graphql/"           graphqlHandler
  post "/api/graphql/:name/"     graphqlByUserHandler

getScoreHandler :: ActionM ()
getScoreHandler = do
  name <- param "name"
  score <- lift $ getScore name
  ok "score" score

getInfoHandler :: ActionM ()
getInfoHandler = do
  name  <- param "name"
  inf  <- lift $ getInfo name
  score <- lift $ getScore name
  json $ object [ "score" .= score, "info" .= inf, "name" .= name ]

setInfoHandler :: ActionM ()
setInfoHandler = do
  name  <- param "name"
  wb <- body
  case (decode wb :: Maybe Value) of
    Nothing -> errBadRequest "Invalid coin info"
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
  okListResult "coins" ListResult { getTotal  = total
                                  , getFrom   = from
                                  , getSize   = size
                                  , getResult = ret
                                  }

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


      ok "score" ret
    Nothing -> errBadRequest "Invalid type"

  where readType :: String -> Maybe CoinType
        readType "Incr" = Just Incr
        readType "Decr" = Just Decr
        readType "incr" = Just Incr
        readType "decr" = Just Decr
        readType "INCR" = Just Incr
        readType "DECR" = Just Decr
        readType _      = Nothing

graphqlHandler :: ActionM ()
graphqlHandler = do
  query <- param "query"
  ret <- lift $ graphql schema query
  json ret

graphqlByUserHandler :: ActionM ()
graphqlByUserHandler = do
  query <- param "query"
  name  <- param "name"
  ret <- lift $ graphql (schemaByUser name) query
  json ret
