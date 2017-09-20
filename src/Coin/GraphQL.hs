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
import           Data.GraphQL.AST      (Name)
import           Data.GraphQL.Schema   (Resolver, Schema, arrayA', object',
                                        objectA, scalar, scalarA)
import           Data.List.NonEmpty    (NonEmpty ((:|)), fromList)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (unpack)
import           Haxl.Core             (GenHaxl)
import           Yuntan.Types.HasMySQL (HasMySQL)

import           Yuntan.Utils.GraphQL  (getIntValue, getTextValue, value')
-- type Query {
--   coin(name: String!): Coin
-- }
-- type Coin {
--   history(from: Int, size: Int): [CoinHistory]
--   total: Int
--   score: Int
--   info: CoinInfo
-- }
-- type CoinHistory {
--   score: Int
--   pre_score: Int
--   type: String
--   desc: String
--   created_at: Int
-- }
-- type CoinInfo {
--
-- }

schema :: HasMySQL u => Schema (GenHaxl u)
schema = coin_ :| []

schemaByUser :: HasMySQL u => String -> Schema (GenHaxl u)
schemaByUser n = fromList (coin__ n)

coin_ :: HasMySQL u => Resolver (GenHaxl u)
coin_ = objectA "coin" $ \argv ->
  case getTextValue "name" argv of
    Nothing   -> empty
    Just name -> coin__ $ unpack name

coin__ :: HasMySQL u => String -> [Resolver (GenHaxl u)]
coin__ n = [ score "score"   n
           , info  "info"    n
           , coins "history" n
           , total "total"   n
           ]

score :: HasMySQL u => Name -> String -> Resolver (GenHaxl u)
score n name = scalarA n . const $ getScore name

info :: HasMySQL u => Name -> String -> Resolver (GenHaxl u)
info n name = object' n $ value' <$> getInfo name


coins :: HasMySQL u => Name -> String -> Resolver (GenHaxl u)
coins n name = arrayA' n $ \argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  map coin <$> getCoinList name from size


total :: HasMySQL u => Name -> String -> Resolver (GenHaxl u)
total n name = scalarA n . const $ countCoin name

coin :: HasMySQL u => Coin -> [Resolver (GenHaxl u)]
coin c = [ scalar "score" $ getCoinScore c
         , scalar "pre_score" $ getCoinPreScore c
         , scalar "type" . show $ getCoinType c
         , scalar "desc" $ getCoinDesc c
         , scalar "created_at" $ getCoinCreatedAt c
         ]
