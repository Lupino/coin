{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Table
  (
    createTable
  ) where

import           Database.MySQL.Simple (Connection, execute_)

import           Data.Int              (Int64)
import           Data.String           (fromString)

import           Coin.Types


createCoinTable :: TablePrefix -> Connection -> IO Int64
createCoinTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_coins` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `score` int(10) unsigned DEFAULT '0',"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createCoinHistoryTable :: TablePrefix -> Connection -> IO Int64
createCoinHistoryTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_coins_history` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `type` varchar(4) NOT NULL,"
                                  , "  `score` int(10) unsigned DEFAULT '0',"
                                  , "  `pre_score` int(10) unsigned DEFAULT '0',"
                                  , "  `desc` varchar(1500) DEFAULT '',"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  KEY `name` (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTable :: TablePrefix -> Connection -> IO Int64
createTable prefix conn = sum <$> mapM (\o -> o prefix conn) [createCoinTable, createCoinHistoryTable]
