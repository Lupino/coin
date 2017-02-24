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
import           Data.GraphQL.Schema    (Argument (..), Resolver, Schema (..),
                                         Value (..), arrayA', objectA', scalar,
                                         scalarA)
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (unpack)

import           Dispatch.Utils.GraphQL (getEnumValue, getIntValue,
                                         getTextValue, value')

schema :: Schema CoinM
schema = Schema [info, score, coins, total]

score :: Resolver CoinM
score = scalarA "score" $ \case
  (Argument "name" (ValueString name):_) -> getScore $ unpack name
  (Argument "name" (ValueEnum name):_)   -> getScore $ unpack name
  _ -> empty

info :: Resolver CoinM
info = objectA' "info" $ \case
  (Argument "name" (ValueString name):_) -> value' <$> getInfo (unpack name)
  (Argument "name" (ValueEnum name):_)   -> value' <$> getInfo (unpack name)
  _ -> empty


coins :: Resolver CoinM
coins = arrayA' "coins" $ \ argv -> do
  let name = catMaybes [ getTextValue "name" argv
                       , getEnumValue "name" argv
                       ]

  case name of
    [] -> empty
    (n:_) -> do
      let from = fromMaybe 0  $ getIntValue "from" argv
          size = fromMaybe 10 $ getIntValue "size" argv

      map coin <$> getCoins (unpack n) from size


total :: Resolver CoinM
total = scalarA "total" $ \case
  (Argument "name" (ValueString name):_) -> countCoin $ unpack name
  (Argument "name" (ValueEnum name):_)   -> countCoin $ unpack name
  _ -> return 0

coin :: Coin -> [Resolver CoinM]
coin c = [ scalar "score" $ getCoinScore c
         , scalar "pre_score" $ getCoinPreScore c
         , scalar "type" . show $ getCoinType c
         , scalar "desc" $ getCoinDesc c
         , scalar "created_at" $ getCoinCreatedAt c
         ]
