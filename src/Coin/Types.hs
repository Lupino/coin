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

  , ListQuery (..)
  , listQueryToTemplate
  , listQueryToAction
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)
import           Database.MySQL.Simple.Param (Action, Param (render))

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
                 , getCoinNameSpace :: String
                 , getCoinDesc      :: Text
                 , getCoinCreatedAt :: CreatedAt
                 }
  deriving (Generic, Eq, Show)

instance Hashable Coin

zeroCoin :: Coin
zeroCoin = Coin { getCoinType      = Incr
                , getCoinScore     = 0
                , getCoinPreScore  = 0
                , getCoinNameSpace = ""
                , getCoinDesc      = ""
                , getCoinCreatedAt = 0
                }

instance QueryResults Coin where
  convertResults [fa, fb, fc, fn, fd, fe]
                 [va, vb, vc, vn, vd, ve] = Coin{..}
    where !getCoinType      = fromMaybe Incr . readMaybe $ convert fa va
          !getCoinScore     = convert fb vb
          !getCoinPreScore  = convert fc vc
          !getCoinNameSpace = convert fn vn
          !getCoinDesc      = convert fd vd
          !getCoinCreatedAt = convert fe ve
  convertResults fs vs  = convertError fs vs 2

instance ToJSON Coin where
  toJSON Coin{..} = object [ "type"       .= show getCoinType
                           , "score"      .= getCoinScore
                           , "pre_score"  .= getCoinPreScore
                           , "namespace"  .= getCoinNameSpace
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

type Name = String
type NameSpace = String

data ListQuery = LQ1 Name
               | LQ2 Name CoinType
               | LQ3 Name NameSpace
               | LQ4 Name CoinType NameSpace
  deriving (Generic, Eq, Show)

instance Hashable ListQuery

listQueryToTemplate :: ListQuery -> String
listQueryToTemplate (LQ1 _) = "`name` = ?"
listQueryToTemplate (LQ2 _ _) = "`name` = ? AND `type` = ?"
listQueryToTemplate (LQ3 _ _) = "`namespace` = ? AND `name` = ?"
listQueryToTemplate (LQ4 _ _ _) = "`namespace` = ? AND `name` = ? AND `type` = ?"

listQueryToAction :: ListQuery -> [Action]
listQueryToAction (LQ1 n) = [render n]
listQueryToAction (LQ2 n t) = [render n, render $ show t]
listQueryToAction (LQ3 n ns) = [render ns, render n]
listQueryToAction (LQ4 n t ns) = [render ns, render n, render $ show t]
