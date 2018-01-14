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
  , lq2T
  , lq2A

  , HistQuery (..)
  , hq2T
  , hq2A
  , modifyHistQuery
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)
import           Database.MySQL.Simple.Param (Action)
import           Database.MySQL.Simple.QueryParams
import           Database.MySQL.Simple  (Only (..))

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

fieldT :: String -> String
fieldT n = "`" ++ n ++ "` = ?"

fieldListT :: [String] -> String
fieldListT [] = []
fieldListT [x] = fieldT x
fieldListT (x:xs) = fieldT x ++ " AND " ++ fieldListT xs

lq2T :: ListQuery -> String
lq2T (LQ1 _) = fieldListT ["name"]
lq2T (LQ2 _ _) = fieldListT ["name", "type"]
lq2T (LQ3 _ _) = fieldListT ["namespace", "name"]
lq2T (LQ4 _ _ _) = fieldListT ["namespace", "name", "type"]

lq2A :: ListQuery -> [Action]
lq2A (LQ1 n) = renderParams (Only n)
lq2A (LQ2 n t) = renderParams (n, show t)
lq2A (LQ3 n ns) = renderParams (ns, n)
lq2A (LQ4 n t ns) = renderParams (ns, n, show t)

data HistQuery = HQ0 Int64 Int64
               | HQ1 Name Int64 Int64
               | HQ2 NameSpace Int64 Int64
               | HQ3 CoinType Int64 Int64
               | HQ4 Name NameSpace Int64 Int64
               | HQ5 Name CoinType Int64 Int64
               | HQ6 NameSpace CoinType Int64 Int64
               | HQ7 Name NameSpace CoinType Int64 Int64
  deriving (Generic, Eq, Show)

instance Hashable HistQuery

fieldHistT :: [String] -> String
fieldHistT [] = "`created_at` > ? AND `created_at` < ?"
fieldHistT (x:xs) = fieldT x ++ " AND " ++ fieldListT xs

hq2T :: HistQuery -> String
hq2T (HQ0 _ _) = fieldHistT []
hq2T (HQ1 _ _ _) = fieldHistT ["name"]
hq2T (HQ2 _ _ _) = fieldHistT ["namespace"]
hq2T (HQ3 _ _ _) = fieldHistT ["type"]
hq2T (HQ4 _ _ _ _) = fieldHistT ["namespace", "name"]
hq2T (HQ5 _ _ _ _) = fieldHistT ["name", "type"]
hq2T (HQ6 _ _ _ _) = fieldHistT ["namespace", "type"]
hq2T (HQ7 _ _ _ _ _) = fieldHistT ["namespace", "name", "type"]

hq2A :: HistQuery -> [Action]
hq2A (HQ0 s e) = renderParams (s, e)
hq2A (HQ1 n s e) = renderParams (n, s, e)
hq2A (HQ2 ns s e) = renderParams (ns, s, e)
hq2A (HQ3 t s e) = renderParams (show t, s, e)
hq2A (HQ4 n ns s e) = renderParams (ns, n, s, e)
hq2A (HQ5 n t s e) = renderParams (n, show t, s, e)
hq2A (HQ6 ns t s e) = renderParams (ns, show t, s, e)
hq2A (HQ7 n ns t s e) = renderParams (ns, n, show t, s, e)

modifyHistQuery :: HistQuery -> Int64 -> Int64 -> HistQuery
modifyHistQuery (HQ0 _ _) = HQ0
modifyHistQuery (HQ1 n _ _) = HQ1 n
modifyHistQuery (HQ2 ns _ _) = HQ2 ns
modifyHistQuery (HQ3 t _ _) = HQ3 t
modifyHistQuery (HQ4 n ns _ _) = HQ4 n ns
modifyHistQuery (HQ5 n t _ _) = HQ5 n t
modifyHistQuery (HQ6 ns t _ _) = HQ6 ns t
modifyHistQuery (HQ7 n ns t _ _) = HQ7 n ns t
