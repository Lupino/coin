{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Coin
import qualified Coin.Config                          as C
import           Coin.Handler
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Data.String                          (fromString)
import qualified Data.Yaml                            as Y
import           Database.PSQL.Types                  (HasPSQL, simpleEnv)
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Options.Applicative
import           Web.Scotty.Haxl                      (ScottyH)
import           Web.Scotty.Trans                     (get, middleware, post,
                                                       put, scottyOptsT,
                                                       settings)

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
  (Right conf) <- Y.decodeFileEither confFile

  let psqlConfig  = C.psqlConfig conf
      psqlThreads = C.psqlHaxlNumThreads psqlConfig

  pool <- C.genPSQLPool psqlConfig

  let state = stateSet (initCoinState psqlThreads) stateEmpty

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }
      u = simpleEnv pool (fromString prefix) ()

  runIO u state mergeData
  scottyOptsT opts (runIO u state) application
  where
        runIO :: HasPSQL u => u -> StateStore -> GenHaxl u w b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: HasPSQL u => ScottyH u w ()
application = do
  middleware logStdout

  get  "/api/coins/:name/score/" getScoreHandler
  get  "/api/coins/:name/info/"  getInfoHandler
  put  "/api/coins/:name/info/"  setInfoHandler
  get  "/api/coins/:name/"       getCoinListHandler
  get  "/api/coins/:name/namespace/:namespace/" getCoinListWithNameSpaceHandler
  post "/api/coins/:name/drop/"  dropCoinHandler
  get  "/api/histories/"            getHistoriesHandler
  get  "/api/histories/:namespace/" getHistoriesByNameSpaceHandler
  post "/api/coins/:name/"       saveCoinHandler
  post "/api/graphql/"           graphqlHandler
  post "/api/graphql/:name/"     graphqlByUserHandler
