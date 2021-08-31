{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Coin.GraphQL
  ( schema
  , schemaByUser
  ) where

import           Coin.API
import           Coin.Types
import           Control.Applicative (Alternative (..), empty)
import qualified Data.GraphQL.AST    as G (Name)
import           Data.GraphQL.Schema (Resolver, Schema, arrayA', object',
                                      objectA, scalar, scalarA)
import           Data.GraphQL.Utils  (getInt, getText, value')
import           Data.List.NonEmpty  (NonEmpty ((:|)), fromList)
import           Data.Maybe          (fromMaybe)
import           Data.String         (fromString)
import           Data.Text           (unpack)
import           Data.UnixTime
import           Database.PSQL.Types (HasPSQL)
import           Haxl.Core           (GenHaxl, throw)
import           Haxl.Core.Monad     (unsafeLiftIO)
import           Haxl.Prelude        (NotFound (..), catchAny)

-- type Query {
--   coin(name: String!): Coin
--   history(
--        namespace: String
--      , name: String
--      , type: Type
--      , start_time: Int
--      , end_time: Int
--      , from: Int
--      , size: Int): [CoinItem]
--   history_count(
--        namespace: String
--      , name: String
--      , type: Type
--      , start_time: Int
--      , end_time: Int): [CoinItem]
-- }
-- type Coin {
--   history(namespace: String, type: Type, from: Int, size: Int): [CoinItem]
--   total(namespace: String, type: Type): Int
--   score: Int
--   info: CoinInfo
-- }
-- type CoinItem {
--   score: Int
--   pre_score: Int
--   type: String
--   namespace: String
--   desc: String
--   created_at: Int
-- }
-- type CoinInfo {
--   _all: JSON
-- }

instance Alternative (GenHaxl u w) where
  a <|> b = catchAny a b
  empty = throw $ NotFound "mzero"

schema :: HasPSQL u => Schema (GenHaxl u w)
schema = coin_ :| [history, historyCount_]

schemaByUser :: HasPSQL u => Name -> Schema (GenHaxl u w)
schemaByUser n = fromList (coin__ n)

coin_ :: HasPSQL u => Resolver (GenHaxl u w)
coin_ = objectA "coin" $ maybe empty coin__ . getName

coin__ :: HasPSQL u => Name -> [Resolver (GenHaxl u w)]
coin__ n = [ score "score"   n
           , info  "info"    n
           , coins "history" n
           , historyCount "history_count" n
           ]

score :: HasPSQL u => G.Name -> Name -> Resolver (GenHaxl u w)
score n name = scalarA n . const $ getScore name

info :: HasPSQL u => G.Name -> Name -> Resolver (GenHaxl u w)
info n name = object' n $ value' <$> getInfo name

getType argv = case getText "type" argv of
                 Nothing -> Nothing
                 Just t  -> readType $ unpack t

getNameSpace argv = case getText "namespace" argv of
                      Nothing -> Nothing
                      Just ns -> Just . fromString $ unpack ns

getName argv = case getText "name" argv of
                 Nothing -> Nothing
                 Just n  -> Just . fromString $ unpack n

getListQuery argv = case (getNameSpace argv, getType argv) of
                      (Nothing, Nothing) -> LQ1
                      (Nothing, Just t)  -> LQ2 t
                      (Just ns, Nothing) -> LQ3 ns
                      (Just ns, Just t)  -> LQ4 t ns

coins :: HasPSQL u => G.Name -> Name -> Resolver (GenHaxl u w)
coins n name = arrayA' n $ \argv -> do
  let from = fromMaybe 0  $ getInt "from" argv
      size = fromMaybe 10 $ getInt "size" argv
      lq = getListQuery argv

  map coin <$> getCoinList (lq name) from size

historyCount :: HasPSQL u => G.Name -> Name -> Resolver (GenHaxl u w)
historyCount n name = scalarA n $ \argv -> countCoin (getListQuery argv name)

coin :: HasPSQL u => Coin -> [Resolver (GenHaxl u w)]
coin c = [ scalar "score" $ getCoinScore c
         , scalar "pre_score" $ getCoinPreScore c
         , scalar "type" . show $ getCoinType c
         , scalar "namespace" $ getCoinNameSpace c
         , scalar "desc" $ getCoinDesc c
         , scalar "created_at" $ getCoinCreatedAt c
         ]

getHistQuery' argv =
  case (getNameSpace argv, getName argv, getType argv) of
    (Nothing, Nothing, Nothing) -> HQ0
    (Nothing, Just n,  Nothing) -> HQ1 n
    (Just ns, Nothing, Nothing) -> HQ2 ns
    (Nothing, Nothing, Just t)  -> HQ3 t
    (Just ns, Just n,  Nothing) -> HQ4 n ns
    (Nothing, Just n,  Just t)  -> HQ5 n t
    (Just ns, Nothing, Just t)  -> HQ6 ns t
    (Just ns, Just n,  Just t)  -> HQ7 n ns t

getHistQuery now argv = getHistQuery' argv startTime endTime
  where startTime = fromMaybe 0 $ getInt "start_time" argv
        endTime = fromMaybe now $ getInt "end_time" argv

history :: HasPSQL u => Resolver (GenHaxl u w)
history = arrayA' "history" $ \argv -> do
  now <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  let from = fromMaybe 0  $ getInt "from" argv
      size = fromMaybe 10 $ getInt "size" argv
      hq = getHistQuery now argv

  map history_ <$> getHistories hq from size

history_ :: HasPSQL u => CoinHistory -> [Resolver (GenHaxl u w)]
history_ h =
  [ scalar "name" $ hCoinName h
  , scalar "score" $ hCoinScore h
  , scalar "pre_score" $ hCoinPreScore h
  , scalar "type" . show $ hCoinType h
  , scalar "namespace" $ hCoinNameSpace h
  , scalar "desc" $ hCoinDesc h
  , scalar "created_at" $ hCoinCreatedAt h
  ]

historyCount_ :: HasPSQL u => Resolver (GenHaxl u w)
historyCount_ = scalarA "history_count" $ \argv -> do
  now <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  countHistory (getHistQuery now argv)
