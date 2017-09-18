module Coin.UserEnv
  (
    UserEnv(..)
  , CoinM
  ) where

import           Data.Pool             (Pool)
import           Database.MySQL.Simple (Connection)

import           Haxl.Core             (GenHaxl)
import qualified Yuntan.Types.HasMySQL as H

data UserEnv = UserEnv { mySQLPool   :: Pool Connection
                       , tablePrefix :: String
                       }

instance H.HasMySQL UserEnv where
  mysqlPool = mySQLPool
  tablePrefix = tablePrefix

type CoinM = GenHaxl UserEnv
