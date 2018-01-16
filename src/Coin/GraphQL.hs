{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Coin.GraphQL
  (
    schema
  , schemaByUser
  ) where

import           Coin.API
import           Coin.Types
import           Control.Applicative   (empty)
import qualified Data.GraphQL.AST as G (Name)
import           Data.GraphQL.Schema   (Resolver, Schema, arrayA', object',
                                        objectA, scalar, scalarA)
import           Data.List.NonEmpty    (NonEmpty ((:|)), fromList)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (unpack)
import           Haxl.Core             (GenHaxl)
import           Haxl.Core.Monad       (unsafeLiftIO)
import           Yuntan.Types.HasMySQL (HasMySQL)
import           Data.UnixTime
import           Data.String           (fromString)

import           Yuntan.Utils.GraphQL  (getIntValue, getTextValue, value')

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

schema :: HasMySQL u => Schema (GenHaxl u)
schema = coin_ :| [history, historyCount_]

schemaByUser :: HasMySQL u => Name -> Schema (GenHaxl u)
schemaByUser n = fromList (coin__ n)

coin_ :: HasMySQL u => Resolver (GenHaxl u)
coin_ = objectA "coin" $ \argv ->
  case getName argv of
    Nothing   -> empty
    Just name -> coin__ name

coin__ :: HasMySQL u => Name -> [Resolver (GenHaxl u)]
coin__ n = [ score "score"   n
           , info  "info"    n
           , coins "history" n
           , historyCount "history_count" n
           ]

score :: HasMySQL u => G.Name -> Name -> Resolver (GenHaxl u)
score n name = scalarA n . const $ getScore name

info :: HasMySQL u => G.Name -> Name -> Resolver (GenHaxl u)
info n name = object' n $ value' <$> getInfo name

getType argv = case getTextValue "type" argv of
                 Nothing -> Nothing
                 Just t -> readType $ unpack t

getNameSpace argv = case getTextValue "namespace" argv of
                      Nothing -> Nothing
                      Just ns -> Just . fromString $ unpack ns

getName argv = case getTextValue "name" argv of
                 Nothing -> Nothing
                 Just n -> Just . fromString $ unpack n

getListQuery argv = case (getNameSpace argv, getType argv) of
                      (Nothing, Nothing) -> LQ1
                      (Nothing, Just t)  -> LQ2 t
                      (Just ns, Nothing) -> LQ3 ns
                      (Just ns, Just t)  -> LQ4 t ns

coins :: HasMySQL u => G.Name -> Name -> Resolver (GenHaxl u)
coins n name = arrayA' n $ \argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv
      lq = getListQuery argv

  map coin <$> getCoinList (lq name) from size

historyCount :: HasMySQL u => G.Name -> Name -> Resolver (GenHaxl u)
historyCount n name = scalarA n $ \argv -> countCoin (getListQuery argv name)

coin :: HasMySQL u => Coin -> [Resolver (GenHaxl u)]
coin c = [ scalar "score" $ getCoinScore c
         , scalar "pre_score" $ getCoinPreScore c
         , scalar "type" . show $ getCoinType c
         , scalar "namespace" $ getCoinNameSpace c
         , scalar "desc" $ getCoinDesc c
         , scalar "created_at" $ getCoinCreatedAt c
         ]

getHistQuery argv = case (getNameSpace argv, getName argv, getType argv) of
                      (Nothing, Nothing, Nothing) -> HQ0
                      (Nothing, Just n,  Nothing) -> HQ1 n
                      (Just ns, Nothing, Nothing) -> HQ2 ns
                      (Nothing, Nothing, Just t)  -> HQ3 t
                      (Just ns, Just n,  Nothing) -> HQ4 n ns
                      (Nothing, Just n,  Just t)  -> HQ5 n t
                      (Just ns, Nothing, Just t)  -> HQ6 ns t
                      (Just ns, Just n,  Just t)  -> HQ7 n ns t


history :: HasMySQL u => Resolver (GenHaxl u)
history = arrayA' "history" $ \argv -> do
  now <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv
      startTime = fromMaybe 0 $ getIntValue "start_time" argv
      endTime = fromMaybe now $ getIntValue "end_time" argv
      hq = getHistQuery argv

  map history_ <$> getCoinHistory (hq startTime endTime) from size

history_ :: HasMySQL u => CoinHistory -> [Resolver (GenHaxl u)]
history_ h =
  [ scalar "name" $ hCoinName h
  , scalar "score" $ hCoinScore h
  , scalar "pre_score" $ hCoinPreScore h
  , scalar "type" . show $ hCoinType h
  , scalar "namespace" $ hCoinNameSpace h
  , scalar "desc" $ hCoinDesc h
  , scalar "created_at" $ hCoinCreatedAt h
  ]

historyCount_ :: HasMySQL u => Resolver (GenHaxl u)
historyCount_ = scalarA "history_count" $ \argv -> do
  now <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  let startTime = fromMaybe 0 $ getIntValue "start_time" argv
      endTime = fromMaybe now $ getIntValue "end_time" argv
      hq = getHistQuery argv
  countCoinHistory (hq startTime endTime)
