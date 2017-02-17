{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Coin.Config
  (
    MySQLConfig (..)
  , Config (..)
  , genMySQLPool
  ) where

import           Data.Aeson            (FromJSON, parseJSON, withObject, (.!=),
                                        (.:), (.:?))
import           Data.Pool             (Pool, createPool)
import           Data.Time             (NominalDiffTime)
import           Database.MySQL.Simple (ConnectInfo (..), Connection, close,
                                        connect, defaultConnectInfo)
import           GHC.Word              (Word16)

data MySQLConfig = MySQLConfig { mysqlDBName           :: String
                               , mysqlHost             :: String
                               , mysqlPort             :: Word16
                               , mysqlUser             :: String
                               , mysqlPass             :: String
                               , mysqlPoolNumStrips    :: Int
                               -- ^ The number of stripes (distinct sub-pools) to maintain.
                               -- The smallest acceptable value is 1.
                               , mysqlPoolIdleTime     :: NominalDiffTime
                               -- ^ Amount of time for which an unused resource is kept alive.
                               -- The smallest acceptable value is 0.5 seconds.
                               --
                               -- The elapsed time before closing may be a little longer than
                               -- requested, as the reaper thread wakes at 1-second intervals.
                               , mysqlPoolMaxResources :: Int
                               -- ^ Maximum number of resources to maintain per stripe.  The
                               -- smallest acceptable value is 1.
                               --
                               -- Requests for resources will block if this limit is reached on a
                               -- single stripe, even if other stripes have idle resources
                               -- available.
                               , mysqlHaxlNumThreads   :: Int
                               -- numThreads of fetch async for haxl
                               }
  deriving (Show)

instance FromJSON MySQLConfig where
  parseJSON = withObject "MySQLConfig" $ \o -> do
    mysqlDBName           <- o .:  "db"
    mysqlHost             <- o .:? "host"         .!= "127.0.0.1"
    mysqlPort             <- o .:? "port"         .!= 3306
    mysqlUser             <- o .:? "user"         .!= "root"
    mysqlPass             <- o .:? "pass"         .!= ""
    mysqlPoolNumStrips    <- o .:? "numStripes"   .!= 1
    mysqlPoolIdleTime     <- o .:? "idleTime"     .!= 0.5
    mysqlPoolMaxResources <- o .:? "maxResources" .!= 1
    mysqlHaxlNumThreads   <- o .:? "numThreads"   .!= 1
    return MySQLConfig{..}

genMySQLPool :: MySQLConfig -> IO (Pool Connection)
genMySQLPool conf = createPool conn close numStripes idleTime maxResources
  where conn = connect defaultConnectInfo { connectDatabase = dbName
                                          , connectHost     = dbHost
                                          , connectPort     = dbPort
                                          , connectUser     = dbUser
                                          , connectPassword = dbPass
                                          }

        dbName       = mysqlDBName conf
        dbHost       = mysqlHost   conf
        dbPort       = mysqlPort   conf
        dbUser       = mysqlUser   conf
        dbPass       = mysqlPass   conf

        numStripes   = mysqlPoolNumStrips    conf
        idleTime     = mysqlPoolIdleTime     conf
        maxResources = mysqlPoolMaxResources conf

data Config = Config { mysqlConfig :: MySQLConfig
                     }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig <- o .: "mysql"
    return Config{..}
