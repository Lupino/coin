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
import           Data.Aeson          (Value (..))
import           Data.GraphQL.AST    (Name)
import           Data.GraphQL.Schema (Argument (..), Resolver, Schema (..),
                                      Value (..), arrayA', scalar, scalarA)
import           Data.Int            (Int32)
import           Data.Maybe          (fromMaybe)
import           Data.String         (fromString)
import           Data.Text           (Text, unpack)
import           Haxl.Core           (CriticalError (..), GenHaxl, catch, throw)
import           Haxl.Core.Monad     (unsafeLiftIO)
import           Prelude             hiding (catch, empty)

catchAny
  :: GenHaxl u a   -- ^ run this first
  -> GenHaxl u a   -- ^ if it throws 'LogicError' or 'TransientError', run this
  -> GenHaxl u a
catchAny haxl handler =
  haxl `catch` \(e :: SomeException) -> handler

instance Alternative (GenHaxl u) where
  a <|> b = catchAny a b
  empty = throw $ CriticalError "mzero"

schema :: Schema CoinM
schema = Schema [info, score, coins, total]

score :: Resolver CoinM
score = scalarA "score" $ \case
  (Argument "name" (ValueString name):_) -> getScore $ unpack name
  (Argument "name" (ValueEnum name):_)   -> getScore $ unpack name
  _ -> return 0

info :: Resolver CoinM
info = scalarA "info" $ \case
  (Argument "name" (ValueString name):_) -> getInfo $ unpack name
  (Argument "name" (ValueEnum name):_)   -> getInfo $ unpack name
  _ -> return Null

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
