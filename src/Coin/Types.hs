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
  , CoinType (..)
  , Coin (..)
  , zeroCoin

  , CoinHistory (..)
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (ToJSON (..),
                                                     Value (String),
                                                     decodeStrict, object, (.=))

import           Data.Hashable                      (Hashable (..))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (encodeUtf8)
import           GHC.Generics                       (Generic)
import           Text.Read                          (readMaybe)

type From        = Int64
type Size        = Int64
type Score       = Int64
type CreatedAt   = Int64

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
                           , "desc"       .= decode getCoinDesc
                           , "created_at" .= getCoinCreatedAt
                           ]

data CoinHistory = CoinHistory
  { hCoinName      :: String
  , hCoinNameSpace :: String
  , hCoinType      :: CoinType
  , hCoinScore     :: Score
  , hCoinPreScore  :: Score
  , hCoinDesc      :: Text
  , hCoinCreatedAt :: CreatedAt
  }
  deriving (Show)


instance QueryResults CoinHistory where
  convertResults [fa, fb, fc, fd, fe, ff, fg]
                 [va, vb, vc, vd, ve, vf, vg] = CoinHistory{..}
    where !hCoinName      = convert fa va
          !hCoinNameSpace = convert fb vb
          !hCoinType      = fromMaybe Incr . readMaybe $ convert fc vc
          !hCoinScore     = convert fd vd
          !hCoinPreScore  = convert fe ve
          !hCoinDesc      = convert ff vf
          !hCoinCreatedAt = convert fg vg
  convertResults fs vs  = convertError fs vs 2

instance ToJSON CoinHistory where
  toJSON CoinHistory{..} = object
    [ "name"       .= hCoinName
    , "namespace"  .= hCoinNameSpace
    , "type"       .= show hCoinType
    , "score"      .= hCoinScore
    , "pre_score"  .= hCoinPreScore
    , "desc"       .= decode hCoinDesc
    , "created_at" .= hCoinCreatedAt
    ]

decode :: Text -> Value
decode v = fromMaybe (String v) $ decodeStrict $ encodeUtf8 v
