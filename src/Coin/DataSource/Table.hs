{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Table
  (
    mergeData
  ) where

import           Database.MySQL.Simple (execute_)

import           Control.Monad         (void)
import           Data.String           (fromString)
import           Yuntan.Types.HasMySQL (MySQL, VersionList, mergeDatabase)

createCoinTable :: MySQL ()
createCoinTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_coins` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `info` varchar(1500) DEFAULT '',"
                                  , "  `score` int(10) unsigned DEFAULT '0',"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createCoinHistoryTable :: MySQL ()
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

updateTable_1510630539 :: MySQL ()
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

updateTable_20180620 :: MySQL ()
updateTable_20180620 prefix conn = do
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_coins_history`"
    , " MODIFY COLUMN `score` bigint(20) unsigned DEFAULT '0'"
    ]
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_coins_history`"
    , " MODIFY COLUMN `pre_score` bigint(20) unsigned DEFAULT '0'"
    ]
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_coins`"
    , " MODIFY COLUMN `score` bigint(20) unsigned DEFAULT '0'"
    ]


versionList :: VersionList
versionList =
  [ (1, [createCoinTable, createCoinHistoryTable])
  , (2, [updateTable_1510630539])
  , (3, [updateTable_20180620])
  ]

mergeData :: MySQL ()
mergeData = mergeDatabase versionList
