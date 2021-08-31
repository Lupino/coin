{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Coin.Config
  ( PSQL (..)
  , Config (..)
  , genPSQLPool
  ) where

import           Data.Aeson           (FromJSON, parseJSON, withObject, (.:))
import           Database.PSQL.Config (PSQL (..), genPSQLPool)

newtype Config = Config
  { psqlConfig :: PSQL
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    psqlConfig <- o .: "psql"
    return Config{..}
