{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Coin.GraphQL
  (
    schema
  ) where

import           Coin.API
import           Coin.Types
import           Coin.UserEnv        (CoinM)
import           Control.Applicative (Alternative (..))
import           Control.Exception   (SomeException)
import           Data.Aeson          as A (Value (..))
import           Data.GraphQL.AST    (Name)
import           Data.GraphQL.Schema (Argument (..), Resolver, Schema (..),
                                      Value (..), arrayA', object, objectA',
                                      scalar, scalarA)
import qualified Data.HashMap.Strict as HM (toList)
import           Data.Int            (Int32)
import           Data.Maybe          (fromMaybe)
import           Data.String         (fromString)
import           Data.Text           (Text, unpack)
import           Haxl.Core           (GenHaxl, throw)
import           Haxl.Prelude        (NotFound (..), catchAny)

instance Alternative (GenHaxl u) where
  a <|> b = catchAny a b
  empty = throw $ NotFound "mzero"

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

getInt :: Name -> [Argument] -> Maybe Int32
getInt _ []                           = Nothing
getInt k (Argument n (ValueInt v):xs) | k == n = Just v
                                      | otherwise = getInt k xs
getInt k (_:xs) = getInt k xs

getText :: Name -> [Argument] -> Maybe Text
getText _ []                           = Nothing

getText k (Argument n (ValueString v):xs) | k == n = Just v
                                          | otherwise = getText k xs

getText k (Argument n (ValueEnum v):xs) | k == n = Just v
                                        | otherwise = getText k xs
getText k (_:xs) = getText k xs

coins :: Resolver CoinM
coins = arrayA' "coins" $ \ argv -> do
  case getText "name" argv of
    Nothing -> empty
    Just name -> do
      let from = fromIntegral . fromMaybe 0 $ getInt "from" argv
          size = fromIntegral . fromMaybe 10 $ getInt "size" argv

      map coin <$> getCoins (unpack name) from size


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

value :: Alternative f => Name -> A.Value -> Resolver f
value k (A.Object v) = object k . listToResolver $ HM.toList v
value k v            = scalar k v

value' :: Alternative f => A.Value -> [Resolver f]
value' (A.Object v) = listToResolver $ HM.toList v
value' _            = []

listToResolver :: Alternative f => [(Text, A.Value)] -> [Resolver f]
listToResolver []          = []
listToResolver ((k, v):xs) = value k v : listToResolver xs
