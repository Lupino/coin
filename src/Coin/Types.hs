{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Coin.Types
  ( Score
  , CreatedAt
  , CoinType (..)
  , Coin (..)
  , zeroCoin
  , readType

  , CoinHistory (..)

  , ListQuery (..)
  , lq2T

  , HistQuery (..)
  , hq2T

  , Name
  , NameSpace
  ) where

import           Data.Aeson          (ToJSON (..), Value (String), decodeStrict,
                                      object, (.=))
import           Data.Hashable       (Hashable (..))
import           Data.Int            (Int64)
import           Data.Maybe          (fromMaybe)
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import           Data.Text.Encoding  (encodeUtf8)
import           Database.PSQL.Types (FromRow (..), Only (..), ToField (..),
                                      ToRow (..), field)
import           GHC.Generics        (Generic)
import           Text.Read           (readMaybe)
import           Web.Scotty.Trans    (Parsable (..))

type From        = Int64
type Size        = Int64
type Score       = Int64
type CreatedAt   = Int64

newtype Name = Name {unName :: String}
  deriving (Generic, Eq, Show)

instance Hashable Name

instance IsString Name where
  fromString = Name

instance ToField Name where
  toField (Name n) = toField n

instance ToJSON Name where
  toJSON = toJSON . unName

instance Parsable Name where
  parseParam t = case parseParam t of
                   Left e  -> Left e
                   Right v -> Right $ Name v

newtype NameSpace = NS {unNS :: String}
  deriving (Generic, Eq, Show)

instance Hashable NameSpace

instance IsString NameSpace where
  fromString = NS

instance ToField NameSpace where
  toField (NS ns) = toField ns

instance ToJSON NameSpace where
  toJSON = toJSON . unNS

instance Parsable NameSpace where
  parseParam t = case parseParam t of
                   Left e  -> Left e
                   Right v -> Right $ NS v

data CoinType = Incr | Decr
  deriving (Generic, Eq, Show, Read)

instance Hashable CoinType

readType :: String -> Maybe CoinType
readType "Incr" = Just Incr
readType "Decr" = Just Decr
readType "incr" = Just Incr
readType "decr" = Just Decr
readType "INCR" = Just Incr
readType "DECR" = Just Decr
readType _      = Nothing

data Coin = Coin
  { getCoinType      :: CoinType
  , getCoinScore     :: Score
  , getCoinPreScore  :: Score
  , getCoinNameSpace :: NameSpace
  , getCoinDesc      :: Text
  , getCoinCreatedAt :: CreatedAt
  }
  deriving (Generic, Eq, Show)

instance Hashable Coin

zeroCoin :: Coin
zeroCoin = Coin
  { getCoinType      = Incr
  , getCoinScore     = 0
  , getCoinPreScore  = 0
  , getCoinNameSpace = ""
  , getCoinDesc      = ""
  , getCoinCreatedAt = 0
  }

instance FromRow Coin where
  fromRow = do
    getCoinType <- fromMaybe Incr . readMaybe <$> field
    getCoinScore <- field
    getCoinPreScore <- field
    getCoinNameSpace <- NS <$> field
    getCoinDesc <- field
    getCoinCreatedAt <- field
    return Coin {..}

instance ToJSON Coin where
  toJSON Coin{..} = object
    [ "type"       .= show getCoinType
    , "score"      .= getCoinScore
    , "pre_score"  .= getCoinPreScore
    , "namespace"  .= getCoinNameSpace
    , "desc"       .= decode getCoinDesc
    , "created_at" .= getCoinCreatedAt
    ]

data CoinHistory = CoinHistory
  { hCoinName      :: String
  , hCoinNameSpace :: NameSpace
  , hCoinType      :: CoinType
  , hCoinScore     :: Score
  , hCoinPreScore  :: Score
  , hCoinDesc      :: Text
  , hCoinCreatedAt :: CreatedAt
  }
  deriving (Show)


instance FromRow CoinHistory where
  fromRow = do
    hCoinName      <- field
    hCoinNameSpace <- NS <$> field
    hCoinType      <- fromMaybe Incr . readMaybe <$> field
    hCoinScore     <- field
    hCoinPreScore  <- field
    hCoinDesc      <- field
    hCoinCreatedAt <- field
    return CoinHistory {..}

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

data ListQuery = LQ1 Name
               | LQ2 CoinType Name
               | LQ3 NameSpace Name
               | LQ4 CoinType NameSpace Name
  deriving (Generic, Eq, Show)

instance Hashable ListQuery

instance ToRow ListQuery where
  toRow (LQ1 n)      = toRow (Only n)
  toRow (LQ2 t n)    = toRow (n, show t)
  toRow (LQ3 ns n)   = toRow (ns, n)
  toRow (LQ4 t ns n) = toRow (ns, n, show t)

fieldT :: String -> String
fieldT n = n ++ " = ?"

fieldListT :: [String] -> String
fieldListT []     = []
fieldListT [x]    = fieldT x
fieldListT (x:xs) = fieldT x ++ " AND " ++ fieldListT xs

lq2T :: ListQuery -> String
lq2T LQ1{} = fieldListT ["name"]
lq2T LQ2{} = fieldListT ["name", "type"]
lq2T LQ3{} = fieldListT ["namespace", "name"]
lq2T LQ4{} = fieldListT ["namespace", "name", "type"]


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
fieldHistT []     = "created_at > ? AND created_at < ?"
fieldHistT (x:xs) = fieldT x ++ " AND " ++ fieldHistT xs

hq2T :: HistQuery -> String
hq2T HQ0{} = fieldHistT []
hq2T HQ1{} = fieldHistT ["name"]
hq2T HQ2{} = fieldHistT ["namespace"]
hq2T HQ3{} = fieldHistT ["type"]
hq2T HQ4{} = fieldHistT ["namespace", "name"]
hq2T HQ5{} = fieldHistT ["name", "type"]
hq2T HQ6{} = fieldHistT ["namespace", "type"]
hq2T HQ7{} = fieldHistT ["namespace", "name", "type"]

instance ToRow HistQuery where
  toRow (HQ0 s e)        = toRow (s, e)
  toRow (HQ1 n s e)      = toRow (n, s, e)
  toRow (HQ2 ns s e)     = toRow (ns, s, e)
  toRow (HQ3 t s e)      = toRow (show t, s, e)
  toRow (HQ4 n ns s e)   = toRow (ns, n, s, e)
  toRow (HQ5 n t s e)    = toRow (n, show t, s, e)
  toRow (HQ6 ns t s e)   = toRow (ns, show t, s, e)
  toRow (HQ7 n ns t s e) = toRow (ns, n, show t, s, e)
