{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Table
  (
    mergeData
  ) where

import           Database.MySQL.Simple (Connection, Only (..), execute_, query_)

import           Control.Monad         (void)
import           Data.Int              (Int64)
import           Data.Maybe            (listToMaybe)
import           Data.String           (fromString)

import           Coin.Types

type Action = TablePrefix -> Connection -> IO ()

createVersionTable :: Action
createVersionTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_version` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `version` int(10) unsigned DEFAULT '0',"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createCoinTable :: Action
createCoinTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_coins` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `info` varchar(1500) DEFAULT '',"
                                  , "  `score` int(10) unsigned DEFAULT '0',"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createCoinHistoryTable :: Action
createCoinHistoryTable prefix conn = void $ execute_ conn sql
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

updateTable_1510630539 :: Action
updateTable_1510630539 prefix conn = do
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_coins_history`"
    , " ADD COLUMN `namespace` varchar(128) DEFAULT 'default'"
    , " AFTER `name`"
    ]
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_coins_history`"
    , " ADD KEY `namespace` (`namespace`)"
    ]

getCurrentVersion :: TablePrefix -> Connection -> IO Int64
getCurrentVersion prefix conn = do
  void $ createVersionTable prefix conn
  ts <- query_ conn $ fromString $ concat
    [ "SELECT `version` FROM `", prefix, "_version`"
    , " WHERE `name` = 'version'"
    ]
  case listToMaybe ts of
    Just ts' -> pure (fromOnly ts')
    Nothing  -> do
      void $ execute_ conn $ fromString $ concat
        [ "INSERT INTO `", prefix, "_version`"
        , " (`name`, `version`)"
        , " VALUES"
        , " ('version', 0)"
        ]
      pure 0


updateVersion :: Int64 -> Action
updateVersion ts prefix conn =
  void $ execute_ conn $ fromString $ concat
    [ "UPDATE `", prefix, "_version`"
    , " SET `version` = ", show ts
    , " WHERE `name` = 'version'"
    ]

versionList :: [(Int64, [Action])]
versionList =
  [ (1, [createCoinTable, createCoinHistoryTable])
  , (2, [updateTable_1510630539])
  ]

mergeData :: TablePrefix -> Connection -> IO Int64
mergeData prefix conn = do
  version <- getCurrentVersion prefix conn
  mapM_ (processAction prefix conn version) versionList
  pure 0

processAction :: TablePrefix -> Connection -> Int64 -> (Int64, [Action]) -> IO ()
processAction prefix conn version (ts, actions) =
  if ts > version then do
                  updateVersion ts prefix conn
                  mapM_ (\o -> o prefix conn) actions
                  else pure ()
