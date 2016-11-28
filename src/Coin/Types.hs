{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Coin.Types
  (
    From
  , Size
  , Score
  , CreatedAt
  , TablePrefix
  , CoinType (..)
  , Coin (..)
  , zeroCoin
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (ToJSON (..), object, (.=))

import           Data.Hashable                      (Hashable (..))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Text.Read                          (readMaybe)

type From        = Int64
type Size        = Int64
type Score       = Int64
type CreatedAt   = Int64
type TablePrefix = String

data CoinType = Incr | Decr
  deriving (Generic, Eq, Show, Read)

instance Hashable CoinType

data Coin = Coin { getCoinType      :: CoinType
                 , getCoinScore     :: Score
                 , getCoinPreScore  :: Score
                 , getCoinDesc      :: Text
                 , getCoinCreatedAt :: CreatedAt
                 }
  deriving (Generic, Eq, Show)

instance Hashable Coin

zeroCoin :: Coin
zeroCoin = Coin { getCoinType      = Incr
                , getCoinScore     = 0
                , getCoinPreScore  = 0
                , getCoinDesc      = ""
                , getCoinCreatedAt = 0
                }

instance QueryResults Coin where
  convertResults [fa, fb, fc, fd, fe]
                 [va, vb, vc, vd, ve] = Coin{..}
    where !getCoinType      = fromMaybe Incr . readMaybe $ convert fa va
          !getCoinScore     = convert fb vb
          !getCoinPreScore  = convert fc vc
          !getCoinDesc      = convert fd vd
          !getCoinCreatedAt = convert fe ve
  convertResults fs vs  = convertError fs vs 2

instance ToJSON Coin where
  toJSON Coin{..} = object [ "type"       .= show getCoinType
                           , "score"      .= getCoinScore
                           , "pre_score"  .= getCoinPreScore
                           , "desc"       .= getCoinDesc
                           , "created_at" .= getCoinCreatedAt
                           ]
