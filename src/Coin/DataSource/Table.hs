{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Table
  ( mergeData
  , coins
  , histories
  ) where

import           Data.Int            (Int64)
import           Database.PSQL.Types (PSQL, TableName, VersionList, createIndex,
                                      createTable, mergeDatabase)

coins :: TableName
coins = "coins"

histories :: TableName
histories = "histories"

createCoinTable :: PSQL Int64
createCoinTable =
  createTable coins
    [ "name VARCHAR(128) PRIMARY KEY"
    , "info VARCHAR(1500) NOT NULL"
    , "score BIGINT DEFAULT '0'"
    ]


createHistoryTable :: PSQL Int64
createHistoryTable =
  createTable histories
    [ "id SERIAL PRIMARY KEY"
    , "name VARCHAR(128) NOT NULL"
    , "namespace varchar(128) DEFAULT 'default'"
    , "type VARCHAR(4) NOT NULL"
    , "score BIGINT DEFAULT '0'"
    , "pre_score BIGINT DEFAULT '0'"
    , "desc VARCHAR(1500) DEFAULT '''"
    , "created_at INT DEFAULT '0'"
    ]

versionList :: VersionList Int64
versionList =
  [ (1,
    [ createCoinTable
    , createHistoryTable
    , createIndex False histories "name" ["name"]
    , createIndex False histories "namespace" ["namespace"]
    ])
  ]

mergeData :: PSQL ()
mergeData = mergeDatabase versionList
