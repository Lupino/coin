{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Coin
  (
    getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoinList
  , countCoin
  , getCoinList'
  , countCoin'

  , getCoinHistory
  , countCoinHistory
  , getCoinHistoryByNameSpace
  , countCoinHistoryByNameSpace

  , dropCoin
  ) where

import           Database.MySQL.Simple  (Only (..), execute, insertID, query,
                                         withTransaction)
import           Yuntan.Types.HasMySQL  (MySQL)

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString, empty)
import           Data.Int               (Int64)
import           Data.Maybe             (listToMaybe)
import           Data.String            (fromString)
import           Data.UnixTime

import           Coin.Types

getScore :: String -> MySQL Score
getScore name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `score` FROM `", prefix, "_coins` WHERE `name` = ?" ]

getInfo :: String -> MySQL ByteString
getInfo name prefix conn = maybe empty fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `info` FROM `", prefix, "_coins` WHERE `name` = ?" ]

hasCoin :: String -> MySQL Bool
hasCoin name prefix conn = exists <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `score` FROM `", prefix, "_coins` WHERE `name` = ?" ]
        exists :: [Only Score] -> Bool
        exists (_:_) = True
        exists []    = False

setInfo :: String -> ByteString -> MySQL ()
setInfo name info prefix conn = do
  exists <- hasCoin name prefix conn
  if exists then void $ execute conn sql (info, name)
            else void $ execute conn insertSQL (name, info)

  where sql = fromString $ concat [ "UPDATE `", prefix, "_coins` SET `info` = ? WHERE `name` = ?" ]
        insertSQL = fromString $ concat [ "INSERT INTO `", prefix, "_coins` (`name`, `info`) VALUES (?, ?)" ]

saveScore :: String -> CoinType -> Score -> MySQL Score
saveScore name tp sc prefix conn = do
  exists <- hasCoin name prefix conn
  if exists then do
              void $ execute conn sql (Only name)
              fromIntegral <$> insertID conn
            else do
              void $ execute conn insertSQL (name, 0 :: Score)
              saveScore name tp sc prefix conn
  where sql = fromString $ concat [ "UPDATE `", prefix, "_coins`"
                                  , " SET `score`=LAST_INSERT_ID(`score` ", getOp tp, show sc,  ")"
                                  , " WHERE `name` = ?"
                                  ]

        getOp :: CoinType -> String
        getOp Incr = "+"
        getOp Decr = "-"

        insertSQL = fromString $ concat [ "INSERT INTO `", prefix, "_coins` (`name`, `score`) VALUES (?, ?)" ]

prepareSaveCoin :: String -> Coin -> MySQL Coin
prepareSaveCoin name coin prefix conn = do
  preScore <- getScore name prefix conn
  ct' <- if ct > 0 then return ct else liftIO $ read . show . toEpochTime <$> getUnixTime
  return coin { getCoinPreScore = preScore
              , getCoinCreatedAt = ct'
              }

  where ct = getCoinCreatedAt coin

saveCoin' :: String -> String -> Coin -> MySQL Int64
saveCoin' namespace name coin prefix conn = execute conn sql (namespace, name, show tp, sc, psc, desc, ct)
  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_coins_history`"
                                  , " (`namespace`, `name`, `type`, `score`, `pre_score`, `desc`, `created_at`)"
                                  , " VALUES"
                                  , " (?, ?, ?, ?, ?, ?, ?)"
                                  ]

        tp   = getCoinType coin
        sc   = getCoinScore coin
        psc  = getCoinPreScore coin
        desc = getCoinDesc coin
        ct   = getCoinCreatedAt coin

saveCoin :: String -> String -> Coin -> MySQL Score
saveCoin namespace name coin prefix conn = withTransaction conn $ do
  coin' <- prepareSaveCoin name coin prefix conn
  changed <- saveCoin' namespace name coin' prefix conn
  if changed > 0 then saveScore name tp sc prefix conn
                 else return 0

  where tp = getCoinType coin
        sc = getCoinScore coin

getCoinList :: String -> From -> Size -> MySQL [Coin]
getCoinList name from size prefix conn = query conn sql ( name, from ,size )
  where sql = fromString $ concat [ "SELECT"
                                  , " `type`, `score`, `pre_score`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `name` = ? ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoin :: String -> MySQL Int64
countCoin name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_coins_history` WHERE `name` = ?" ]

getCoinList' :: CoinType -> String -> From -> Size -> MySQL [Coin]
getCoinList' tp name from size prefix conn = query conn sql (name, show tp, from, size)
  where sql = fromString $ concat [ "SELECT"
                                  , " `type`, `score`, `pre_score`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `name` = ? AND `type` = ?"
                                  , " ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoin' :: CoinType -> String -> MySQL Int64
countCoin' tp name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (name, show tp)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_coins_history`"
                                  , " WHERE `name` = ? AND `type` = ?"
                                  ]

getCoinHistory :: Int64 -> Int64 -> From -> Size -> MySQL [CoinHistory]
getCoinHistory start end from size prefix conn = query conn sql (start, end, from ,size)
  where sql = fromString $ concat [ "SELECT"
                                  , " `name`, `namespace`, `type`, `score`, `pre_score`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `created_at` > ? AND `created_at` < ?"
                                  , " ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoinHistory :: Int64 -> Int64 -> MySQL Int64
countCoinHistory start end prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (start, end)
  where sql = fromString $ concat [ "SELECT"
                                  , " count(*)"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `created_at` > ? AND `created_at` < ?"
                                  ]


getCoinHistoryByNameSpace :: String -> Int64 -> Int64 -> From -> Size -> MySQL [CoinHistory]
getCoinHistoryByNameSpace namespace start end from size prefix conn = query conn sql (namespace, start, end, from ,size)
  where sql = fromString $ concat [ "SELECT"
                                  , " `name`, `namespace`, `type`, `score`, `pre_score`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `namespace` = ? AND `created_at` > ? AND `created_at` < ?"
                                  , " ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoinHistoryByNameSpace :: String -> Int64 -> Int64 -> MySQL Int64
countCoinHistoryByNameSpace namespace start end prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (namespace, start, end)
  where sql = fromString $ concat [ "SELECT"
                                  , " count(*)"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `namespace` = ? AND `created_at` > ? AND `created_at` < ?"
                                  ]

dropCoin :: String -> MySQL ()
dropCoin name prefix conn = do
  void $ execute conn sql (Only name)
  void $ execute conn sql1 (Only name)
  where sql  = fromString $ concat [ "DELETE FROM `", prefix, "_coins` WHERE `name`=?"]
        sql1 = fromString $ concat [ "DELETE FROM `", prefix, "_coins_history` WHERE `name`=?"]
