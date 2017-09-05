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

import           Data.Aeson            (Value (..), decodeStrict, encode)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Int              (Int64)
import           Data.Maybe            (fromMaybe)
import           Haxl.Core             (GenHaxl, dataFetch, uncachedRequest)

import           Coin.DataSource
import           Coin.Types
import           Yuntan.Types.HasMySQL (HasMySQL)

getScore    :: HasMySQL u => String -> GenHaxl u Score
getInfo     :: HasMySQL u => String -> GenHaxl u Value
setInfo     :: HasMySQL u => String -> Value -> GenHaxl u ()
saveCoin    :: HasMySQL u => String -> Coin -> GenHaxl u Score
getCoins    :: HasMySQL u => String -> From -> Size -> GenHaxl u [Coin]
countCoin   :: HasMySQL u => String -> GenHaxl u Int64
createTable :: HasMySQL u => GenHaxl u Int64

getScore n      = uncachedRequest (GetScore n)
getInfo n       = fromMaybe Null . decodeStrict <$> dataFetch (GetInfo n)
setInfo n i     = uncachedRequest . SetInfo n . toStrict $ encode i
saveCoin n c    = uncachedRequest (SaveCoin n c)
getCoins n f si = uncachedRequest (GetCoins n f si)
countCoin n     = uncachedRequest (CountCoin n)
createTable     = uncachedRequest CreateTable
