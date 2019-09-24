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

getScore         :: HasMySQL u => Name -> GenHaxl u w Score
getInfo          :: HasMySQL u => Name -> GenHaxl u w Value
setInfo          :: HasMySQL u => Name -> Value -> GenHaxl u w ()
saveCoin         :: HasMySQL u => NameSpace -> Name -> Coin -> GenHaxl u w Score
getCoinList      :: HasMySQL u => ListQuery -> From -> Size -> GenHaxl u w [Coin]
countCoin        :: HasMySQL u => ListQuery -> GenHaxl u w Int64
mergeData        :: HasMySQL u => GenHaxl u w ()
getCoinHistory   :: HasMySQL u => HistQuery -> From -> Size -> GenHaxl u w [CoinHistory]
countCoinHistory :: HasMySQL u => HistQuery -> GenHaxl u w Int64
dropCoin         :: HasMySQL u => Name -> GenHaxl u w ()

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
