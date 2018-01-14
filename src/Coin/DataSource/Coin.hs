{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Coin
  (
    getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoinList
  , countCoin

  , getCoinHistory
  , countCoinHistory

  , dropCoin
  ) where

import           Database.MySQL.Simple  (Only (..), execute, insertID, query,
                                         withTransaction)
import           Yuntan.Types.HasMySQL  (MySQL)
import           Database.MySQL.Simple.QueryParams

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


getCoinList :: ListQuery -> From -> Size -> MySQL [Coin]
getCoinList lq from size prefix conn = query conn sql $ lq2A lq ++ renderParams (from, size)
  where sql = fromString $ concat [ "SELECT"
                                  , " `type`, `score`, `pre_score`, `namespace`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE ", lq2T lq, " ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoin :: ListQuery -> MySQL Int64
countCoin lq prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (lq2A lq)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_coins_history` WHERE ", lq2T lq]

getCoinHistory :: HistQuery -> From -> Size -> MySQL [CoinHistory]
getCoinHistory hq from size prefix conn = query conn sql $ hq2A hq ++ renderParams (from ,size)
  where sql = fromString $ concat [ "SELECT"
                                  , " `name`, `namespace`, `type`, `score`, `pre_score`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE ", hq2T hq
                                  , " ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoinHistory :: HistQuery -> MySQL Int64
countCoinHistory hq prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (hq2A hq)
  where sql = fromString $ concat [ "SELECT"
                                  , " count(*)"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE ", hq2T hq
                                  ]

dropCoin :: String -> MySQL ()
dropCoin name prefix conn = do
  void $ execute conn sql (Only name)
  void $ execute conn sql1 (Only name)
  where sql  = fromString $ concat [ "DELETE FROM `", prefix, "_coins` WHERE `name`=?"]
        sql1 = fromString $ concat [ "DELETE FROM `", prefix, "_coins_history` WHERE `name`=?"]
