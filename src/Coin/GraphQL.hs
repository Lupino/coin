{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Coin.GraphQL
  (
    schema
  ) where

import           Coin.API
import           Coin.Types
import           Coin.UserEnv           (CoinM)
import           Control.Applicative    (empty)
import           Data.GraphQL.AST       (Name)
import           Data.GraphQL.Schema    (Argument (..), Resolver, Schema,
                                         Value (..), arrayA', object', objectA,
                                         scalar, scalarA)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (unpack)

import           Dispatch.Utils.GraphQL (getEnumValue, getIntValue,
                                         getTextValue, value')
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

schema :: Schema CoinM
schema = coin_ :| []

coin_ :: Resolver CoinM
coin_ = objectA "coin" $ \argv -> do
  case getTextValue "name" argv of
    Nothing   -> empty
    Just name -> coin__ $ unpack name

 where coin__ :: String -> [Resolver CoinM]
       coin__ n = [ score "score"   n
                  , info  "info"    n
                  , coins "history" n
                  , total "total"   n
                  ]

score :: Name -> String -> Resolver CoinM
score n name = scalarA n . const $ getScore name

info :: Name -> String -> Resolver CoinM
info n name = object' n $ value' <$> getInfo name


coins :: Name -> String -> Resolver CoinM
coins n name = arrayA' n $ \argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  map coin <$> getCoins name from size


total :: Name -> String -> Resolver CoinM
total n name = scalarA n . const $ countCoin name

coin :: Coin -> [Resolver CoinM]
coin c = [ scalar "score" $ getCoinScore c
         , scalar "pre_score" $ getCoinPreScore c
         , scalar "type" . show $ getCoinType c
         , scalar "desc" $ getCoinDesc c
         , scalar "created_at" $ getCoinCreatedAt c
         ]
