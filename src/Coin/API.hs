module Coin.API
  (
    getScore
  , saveCoin
  , getCoins
  , countCoin
  , createTable
  ) where

import           Data.Int        (Int64)
import           Haxl.Core       (uncachedRequest)

import           Coin.DataSource
import           Coin.Types
import           Coin.UserEnv    (CoinM)

getScore    :: String -> CoinM Score
saveCoin    :: String -> Coin -> CoinM Score
getCoins    :: String -> From -> Size -> CoinM [Coin]
countCoin   :: String -> CoinM Int64
createTable :: CoinM Int64

getScore n      = uncachedRequest (GetScore n)
saveCoin n c    = uncachedRequest (SaveCoin n c)
getCoins n f si = uncachedRequest (GetCoins n f si)
countCoin n     = uncachedRequest (CountCoin n)
createTable     = uncachedRequest CreateTable
