module Coin.API
  ( getScore
  , getInfo
  , setInfo
  , saveCoin
  , getCoinList
  , countCoin
  , mergeData
  , getHistories
  , countHistory
  , dropCoin
  ) where

import           Coin.DataSource
import           Coin.Types
import           Data.Aeson           (Value (..), decodeStrict, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Int             (Int64)
import           Data.Maybe           (fromMaybe)
import           Database.PSQL.Types  (From, HasPSQL, Size)
import           Haxl.Core            (GenHaxl, dataFetch, uncachedRequest)

getScore     :: HasPSQL u => Name -> GenHaxl u w Score
getInfo      :: HasPSQL u => Name -> GenHaxl u w Value
setInfo      :: HasPSQL u => Name -> Value -> GenHaxl u w Int64
saveCoin     :: HasPSQL u => NameSpace -> Name -> Coin -> GenHaxl u w Score
getCoinList  :: HasPSQL u => ListQuery -> From -> Size -> GenHaxl u w [Coin]
countCoin    :: HasPSQL u => ListQuery -> GenHaxl u w Int64
mergeData    :: HasPSQL u => GenHaxl u w ()
getHistories :: HasPSQL u => HistQuery -> From -> Size -> GenHaxl u w [CoinHistory]
countHistory :: HasPSQL u => HistQuery -> GenHaxl u w Int64
dropCoin     :: HasPSQL u => Name -> GenHaxl u w ()

getScore n         = dataFetch (GetScore n)
getInfo n          = fromMaybe Null . decodeStrict <$> dataFetch (GetInfo n)
setInfo n i        = uncachedRequest . SetInfo n . toStrict $ encode i
saveCoin s n c     = uncachedRequest (SaveCoin s n c)
getCoinList n f si = dataFetch (GetCoinList n f si)
countCoin n        = dataFetch (CountCoin n)
mergeData          = uncachedRequest MergeData
getHistories a b c = dataFetch (GetHistories a b c)
countHistory a     = dataFetch (CountHistory a)
dropCoin a         = uncachedRequest (DropCoin a)
