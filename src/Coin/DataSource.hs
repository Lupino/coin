{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
                                           StateKey, StateStore, dataSourceName,
                                           fetch, putFailure, putSuccess, showp,
                                           stateEmpty, stateSet)

import           Coin.DataSource.Coin
import           Coin.DataSource.Table
import           Coin.Types
import           Coin.UserEnv             (UserEnv (..))

import qualified Control.Exception        (SomeException, bracket_, try)
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.Pool                (withResource)
import           Database.MySQL.Simple    (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data CoinReq a where
  CreateTable :: CoinReq Int64
  GetScore    :: String -> CoinReq Score
  SaveCoin    :: String -> Coin -> CoinReq Score
  GetCoins    :: String -> From -> Size -> CoinReq [Coin]
  CountCoin   :: String -> CoinReq Int64
  GetInfo     :: String -> CoinReq ByteString
  SetInfo     :: String -> ByteString -> CoinReq ()

  deriving (Typeable)

deriving instance Eq (CoinReq a)
instance Hashable (CoinReq a) where
  hashWithSalt s CreateTable       = hashWithSalt s (0::Int)
  hashWithSalt s (GetScore n)      = hashWithSalt s (1::Int, n)
  hashWithSalt s (SaveCoin n c)    = hashWithSalt s (2::Int, n, c)
  hashWithSalt s (GetCoins n f si) = hashWithSalt s (3::Int, n, f, si)
  hashWithSalt s (CountCoin n)     = hashWithSalt s (4::Int, n)
  hashWithSalt s (GetInfo n)       = hashWithSalt s (5::Int, n)
  hashWithSalt s (SetInfo n i)     = hashWithSalt s (6::Int, n, i)

deriving instance Show (CoinReq a)
instance ShowP CoinReq where showp = show

instance StateKey CoinReq where
  data State CoinReq = CoinState { numThreads :: Int }

instance DataSourceName CoinReq where
  dataSourceName _ = "CoinDataSource"

instance DataSource UserEnv CoinReq where
  fetch = doFetch

doFetch
  :: State CoinReq
  -> Flags
  -> UserEnv
  -> [BlockedFetch CoinReq]
  -> PerformFetch

doFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> UserEnv -> BlockedFetch CoinReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mySQLPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch CoinReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: CoinReq a -> TablePrefix -> Connection -> IO a
fetchReq CreateTable       = createTable
fetchReq (GetScore n)      = getScore n
fetchReq (SaveCoin n c)    = saveCoin n c
fetchReq (GetCoins n f si) = getCoins n f si
fetchReq (CountCoin n)     = countCoin n
fetchReq (GetInfo n)       = getInfo n
fetchReq (SetInfo n i)     = setInfo n i

initCoinState :: Int -> StateStore
initCoinState threads = stateSet coinState stateEmpty
  where coinState = CoinState threads
