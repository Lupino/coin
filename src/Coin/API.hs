module Coin.API
  (
    getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoins
  , countCoin
  , createTable
  ) where

import           Data.Aeson           (Value (..), decodeStrict, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Int             (Int64)
import           Data.Maybe           (fromMaybe)
import           Haxl.Core            (dataFetch, uncachedRequest)

import           Coin.DataSource
import           Coin.Types
import           Coin.UserEnv         (CoinM)

getScore    :: String -> CoinM Score
getInfo     :: String -> CoinM Value
setInfo     :: String -> Value -> CoinM ()
saveCoin    :: String -> Coin -> CoinM Score
getCoins    :: String -> From -> Size -> CoinM [Coin]
countCoin   :: String -> CoinM Int64
createTable :: CoinM Int64

getScore n      = uncachedRequest (GetScore n)
getInfo n       = fromMaybe Null . decodeStrict <$> dataFetch (GetInfo n)
setInfo n i     = uncachedRequest . SetInfo n . toStrict $ encode i
saveCoin n c    = uncachedRequest (SaveCoin n c)
getCoins n f si = uncachedRequest (GetCoins n f si)
countCoin n     = uncachedRequest (CountCoin n)
createTable     = uncachedRequest CreateTable
