{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Coin
  (
    getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoins
  , countCoin
  ) where

import           Database.MySQL.Simple  (Connection, Only (..), execute,
                                         insertID, query, withTransaction)

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (..), encode)
import           Data.ByteString        (ByteString, empty)
import           Data.Int               (Int64)
import           Data.Maybe             (listToMaybe)
import           Data.String            (fromString)
import           Data.UnixTime

import           Coin.Types

getScore :: String -> TablePrefix -> Connection -> IO Score
getScore name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `score` FROM `", prefix, "_coins` WHERE `name` = ?" ]

getInfo :: String -> TablePrefix -> Connection -> IO ByteString
getInfo name prefix conn = maybe empty fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `info` FROM `", prefix, "_coins` WHERE `name` = ?" ]

setInfo :: String -> ByteString -> TablePrefix -> Connection -> IO ()
setInfo name info prefix conn = void $ execute conn sql (name, info)
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_coins` (`name`, `info`) VALUES (?, ?)" ]

saveScore :: String -> CoinType -> Score -> TablePrefix -> Connection -> IO Score
saveScore name tp sc prefix conn = do
  changed <- execute conn sql (Only name)
  if changed > 0 then fromIntegral <$> insertID conn
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

prepareSaveCoin :: String -> Coin -> TablePrefix -> Connection -> IO Coin
prepareSaveCoin name coin prefix conn = do
  preScore <- getScore name prefix conn
  ct' <- if ct > 0 then return ct else liftIO $ read . show . toEpochTime <$> getUnixTime
  return coin { getCoinPreScore = preScore
              , getCoinCreatedAt = ct'
              }

  where ct = getCoinCreatedAt coin

saveCoin' :: String -> Coin -> TablePrefix -> Connection -> IO Int64
saveCoin' name coin prefix conn = execute conn sql (name, show tp, sc, psc, desc, ct)
  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_coins_history`"
                                  , " (`name`, `type`, `score`, `pre_score`, `desc`, `created_at`)"
                                  , " VALUES"
                                  , " (?, ?, ?, ?, ?, ?)"
                                  ]

        tp   = getCoinType coin
        sc   = getCoinScore coin
        psc  = getCoinPreScore coin
        desc = getCoinDesc coin
        ct   = getCoinCreatedAt coin

saveCoin :: String -> Coin -> TablePrefix -> Connection -> IO Score
saveCoin name coin prefix conn = withTransaction conn $ do
  coin' <- prepareSaveCoin name coin prefix conn
  changed <- saveCoin' name coin' prefix conn
  if changed > 0 then saveScore name tp sc prefix conn
                 else return 0

  where tp = getCoinType coin
        sc = getCoinScore coin

getCoins :: String -> From -> Size -> TablePrefix -> Connection -> IO [Coin]
getCoins name from size prefix conn = query conn sql ( name, from ,size )
  where sql = fromString $ concat [ "SELECT"
                                  , " `type`, `score`, `pre_score`, `desc`, `created_at`"
                                  , " FROM `", prefix, "_coins_history`"
                                  , " WHERE `name` = ? ORDER BY `id` DESC LIMIT ?,?"
                                  ]

countCoin :: String -> TablePrefix -> Connection -> IO Int64
countCoin name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_coins_history` WHERE `name` = ?" ]

