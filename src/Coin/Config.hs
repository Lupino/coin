{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Coin.Config
  (
    MySQLConfig (..)
  , Config (..)
  , genMySQLPool
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.:))

import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)

data Config = Config { mysqlConfig :: MySQLConfig
                     }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig <- o .: "mysql"
    return Config{..}
