{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Coin.DataSource (
    CoinReq(..),
    initCoinState
  ) where

import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), ShowP, State,
                                           StateKey, dataSourceName, fetch,
                                           putFailure, putSuccess, showp)

import           Coin.DataSource.Coin
import           Coin.DataSource.Table
import           Coin.Types
import           Yuntan.Types.HasMySQL    (HasMySQL, MySQL, mysqlPool,
                                           tablePrefix)

import qualified Control.Exception        (SomeException, bracket_, try)
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.Pool                (withResource)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data CoinReq a where
  MergeData        :: CoinReq ()
  GetScore         :: String -> CoinReq Score
  SaveCoin         :: String -> String -> Coin -> CoinReq Score
  GetCoinList      :: ListQuery -> From -> Size -> CoinReq [Coin]
  CountCoin        :: ListQuery -> CoinReq Int64
  GetInfo          :: String -> CoinReq ByteString
  SetInfo          :: String -> ByteString -> CoinReq ()
  GetCoinHistory   :: HistQuery -> From -> Size -> CoinReq [CoinHistory]
  CountCoinHistory :: HistQuery -> CoinReq Int64
  DropCoin         :: String -> CoinReq ()

  deriving (Typeable)

deriving instance Eq (CoinReq a)
instance Hashable (CoinReq a) where
  hashWithSalt s MergeData              = hashWithSalt s (0::Int)
  hashWithSalt s (GetScore n)           = hashWithSalt s (1::Int, n)
  hashWithSalt s (SaveCoin ns n c)      = hashWithSalt s (2::Int, ns, n, c)
  hashWithSalt s (GetCoinList n f si)   = hashWithSalt s (3::Int, n, f, si)
  hashWithSalt s (CountCoin n)          = hashWithSalt s (4::Int, n)
  hashWithSalt s (GetInfo n)            = hashWithSalt s (5::Int, n)
  hashWithSalt s (SetInfo n i)          = hashWithSalt s (6::Int, n, i)
  hashWithSalt s (GetCoinHistory a b c) = hashWithSalt s (7::Int, a, b, c)
  hashWithSalt s (CountCoinHistory a)   = hashWithSalt s (8::Int, a)
  hashWithSalt s (DropCoin a)           = hashWithSalt s (9::Int, a)

deriving instance Show (CoinReq a)
instance ShowP CoinReq where showp = show

instance StateKey CoinReq where
  data State CoinReq = CoinState { numThreads :: Int }

instance DataSourceName CoinReq where
  dataSourceName _ = "CoinDataSource"

instance HasMySQL u => DataSource u CoinReq where
  fetch = doFetch

doFetch
  :: HasMySQL u
  => State CoinReq
  -> Flags
  -> u
  -> [BlockedFetch CoinReq]
  -> PerformFetch

doFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch CoinReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch CoinReq -> MySQL ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: CoinReq a -> MySQL a
fetchReq MergeData              = mergeData
fetchReq (GetScore n)           = getScore n
fetchReq (SaveCoin s n c)       = saveCoin s n c
fetchReq (GetCoinList n f si)   = getCoinList n f si
fetchReq (CountCoin n)          = countCoin n
fetchReq (GetInfo n)            = getInfo n
fetchReq (SetInfo n i)          = setInfo n i
fetchReq (GetCoinHistory a b c) = getCoinHistory a b c
fetchReq (CountCoinHistory a)   = countCoinHistory a
fetchReq (DropCoin a)           = dropCoin a

initCoinState :: Int -> State CoinReq
initCoinState = CoinState
