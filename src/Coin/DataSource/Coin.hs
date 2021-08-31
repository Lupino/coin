{-# LANGUAGE OverloadedStrings #-}

module Coin.DataSource.Coin
  ( getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoinList
  , countCoin

  , getHistories
  , countHistory

  , dropCoin
  ) where

import           Coin.DataSource.Table  (coins, histories)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import           Data.UnixTime
import           Database.PSQL.Types    (From, Only (..), PSQL, Size, as, count,
                                         delete, desc, insert, insertOrUpdate,
                                         select, selectOneOnly, withTransaction)

import           Coin.Types

getScore_ :: Name -> PSQL (Maybe Score)
getScore_ name = selectOneOnly coins "score" "name=?" (Only name)

getScore :: Name -> PSQL Score
getScore name = fromMaybe 0 <$> getScore_ name

getInfo :: Name -> PSQL ByteString
getInfo name = fromMaybe "" <$> selectOneOnly coins "info" "name=?" (Only name)

setInfo :: Name -> ByteString -> PSQL Int64
setInfo name info = insertOrUpdate coins ["name"] ["info"] [] (name, info)

saveScore :: Name -> CoinType -> Score -> PSQL Score
saveScore name tp sc  = do
  _ <- insertOrUpdate (coins `as` "coin") ["name"] [fromString ("score = coin.score" ++ getOp tp ++ show sc)] [] (name, sc)
  getScore name
  where getOp :: CoinType -> String
        getOp Incr = "+"
        getOp Decr = "-"

prepareSaveCoin :: Name -> Coin -> PSQL Coin
prepareSaveCoin name coin  = do
  preScore <- getScore name
  ct' <- if ct > 0 then return ct else liftIO $ read . show . toEpochTime <$> getUnixTime
  return coin { getCoinPreScore = preScore
              , getCoinCreatedAt = ct'
              }

  where ct = getCoinCreatedAt coin

saveCoin' :: NameSpace -> Name -> Coin -> PSQL Int64
saveCoin' namespace name coin  =
  insert histories
    [ "namespace"
    , "name"
    , "type"
    , "score"
    , "pre_score"
    , "\"desc\""
    , "created_at"
    ] (namespace, name, show tp, sc, psc, des, ct)
  where tp  = getCoinType coin
        sc  = getCoinScore coin
        psc = getCoinPreScore coin
        des = getCoinDesc coin
        ct  = getCoinCreatedAt coin

saveCoin :: NameSpace -> Name -> Coin -> PSQL Score
saveCoin namespace name coin  = withTransaction $ do
  coin' <- prepareSaveCoin name coin
  changed <- saveCoin' namespace name coin'
  if changed > 0 then saveScore name tp sc
                 else return 0

  where tp = getCoinType coin
        sc = getCoinScore coin

getCoinList :: ListQuery -> From -> Size -> PSQL [Coin]
getCoinList lq from size  = select histories
  [ "type"
  , "score"
  , "pre_score"
  , "namespace"
  , "\"desc\""
  , "created_at"
  ] (lq2T lq) lq from size (desc "id")

countCoin :: ListQuery -> PSQL Int64
countCoin lq = count histories (lq2T lq) lq

getHistories :: HistQuery -> From -> Size -> PSQL [CoinHistory]
getHistories hq from size = select histories
  [ "name"
  , "namespace"
  , "type"
  , "score"
  , "pre_score"
  , "\"desc\""
  , "created_at"
  ] (hq2T hq) hq from size (desc "id")

countHistory :: HistQuery -> PSQL Int64
countHistory hq = count histories (hq2T hq) hq

dropCoin :: Name -> PSQL ()
dropCoin name = withTransaction $ do
  void $ delete coins "name=?" (Only name)
  void $ delete histories "name=?" (Only name)
