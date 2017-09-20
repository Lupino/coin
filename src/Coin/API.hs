module Coin.API
  (
    getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoinList
  , countCoin
  , createTable
  , getCoinHistory
  , countCoinHistory
  ) where

import           Data.Aeson            (Value (..), decodeStrict, encode)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Int              (Int64)
import           Data.Maybe            (fromMaybe)
import           Haxl.Core             (GenHaxl, dataFetch, uncachedRequest)

import           Coin.DataSource
import           Coin.Types
import           Yuntan.Types.HasMySQL (HasMySQL)

getScore         :: HasMySQL u => String -> GenHaxl u Score
getInfo          :: HasMySQL u => String -> GenHaxl u Value
setInfo          :: HasMySQL u => String -> Value -> GenHaxl u ()
saveCoin         :: HasMySQL u => String -> Coin -> GenHaxl u Score
getCoinList      :: HasMySQL u => String -> From -> Size -> GenHaxl u [Coin]
countCoin        :: HasMySQL u => String -> GenHaxl u Int64
createTable      :: HasMySQL u => GenHaxl u Int64
getCoinHistory   :: HasMySQL u => Int64 -> Int64 -> From -> Size -> GenHaxl u [CoinHistory]
countCoinHistory :: HasMySQL u => Int64 -> Int64 -> GenHaxl u Int64

getScore n             = uncachedRequest (GetScore n)
getInfo n              = fromMaybe Null . decodeStrict <$> dataFetch (GetInfo n)
setInfo n i            = uncachedRequest . SetInfo n . toStrict $ encode i
saveCoin n c           = uncachedRequest (SaveCoin n c)
getCoinList n f si     = uncachedRequest (GetCoinList n f si)
countCoin n            = uncachedRequest (CountCoin n)
createTable            = uncachedRequest CreateTable
getCoinHistory a b c d = uncachedRequest (GetCoinHistory a b c d)
countCoinHistory a b   = uncachedRequest (CountCoinHistory a b)
