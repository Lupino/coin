module Coin.API
  (
    getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoinList
  , countCoin
  , mergeData
  , getCoinHistory
  , countCoinHistory
  , dropCoin
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
saveCoin         :: HasMySQL u => String -> String -> Coin -> GenHaxl u Score
getCoinList      :: HasMySQL u => ListQuery -> From -> Size -> GenHaxl u [Coin]
countCoin        :: HasMySQL u => ListQuery -> GenHaxl u Int64
mergeData        :: HasMySQL u => GenHaxl u ()
getCoinHistory   :: HasMySQL u => HistQuery -> From -> Size -> GenHaxl u [CoinHistory]
countCoinHistory :: HasMySQL u => HistQuery -> GenHaxl u Int64
dropCoin         :: HasMySQL u => String -> GenHaxl u ()

getScore n           = dataFetch (GetScore n)
getInfo n            = fromMaybe Null . decodeStrict <$> dataFetch (GetInfo n)
setInfo n i          = uncachedRequest . SetInfo n . toStrict $ encode i
saveCoin s n c       = uncachedRequest (SaveCoin s n c)
getCoinList n f si   = dataFetch (GetCoinList n f si)
countCoin n          = dataFetch (CountCoin n)
mergeData            = uncachedRequest MergeData
getCoinHistory a b c = dataFetch (GetCoinHistory a b c)
countCoinHistory a   = dataFetch (CountCoinHistory a)
dropCoin a           = uncachedRequest (DropCoin a)
