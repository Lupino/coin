{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Coin
  (
    saveCoin,
    getLastID,
    getCoins,
    getScore,
    Coin (..),
    Score,
    Created,
    Description
  ) where

import           Data.Aeson            (FromJSON (..), ToJSON (..),
                                        decodeStrict, encode, object,
                                        withObject, (.:), (.=))
import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Data.ByteString.Lazy  (toStrict)
import           Database.Redis        (Connection, Redis, Reply, decrby, get,
                                        incrby, mget, runRedis, set)

import           Control.Monad         (when)
import           Data.Maybe            (fromJust, isJust, mapMaybe)

type Score = Integer
type Created = Integer
type Description = String

data CoinType = Incr | Decr deriving (Show, Read)

data Coin = Coin { getCoinType      :: CoinType
                 , getCoinScore     :: Score
                 , getCoinPreScore  :: Score
                 , getCoinDesc      :: Description
                 , getCoinCreatedAt :: Created
                 }

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \o -> do
    getCoinType      <- read <$> o .: "type"
    getCoinScore     <- o .: "score"
    getCoinPreScore  <- o .: "pre_score"
    getCoinDesc      <- o .: "desc"
    getCoinCreatedAt <- o .: "created_at"
    return Coin{..}

instance ToJSON Coin where
  toJSON Coin{..} = object [ "type"       .= show getCoinType
                           , "score"      .= getCoinScore
                           , "pre_score"  .= getCoinPreScore
                           , "desc"       .= getCoinDesc
                           , "created_at" .= getCoinCreatedAt
                           ]

-- coins:{username}:totalscore
-- coins:{username}:{coinId}
-- coins:{username}:lastid

saveCoin :: String -> Coin -> Redis (Maybe Score)
saveCoin name coin = do
  lastid <- saveCoin' name coin
  case lastid of
    Just _  -> updateScore name coin
    Nothing -> return Nothing

updateScore :: String -> Coin -> Redis (Maybe Score)
updateScore name coin = unpackIntegerScore <$> getAction t scorekey score
  where scorekey = pack $ "coins:" ++ name ++ ":totalscore"
        score = getCoinScore coin
        t = getCoinType coin
        getAction :: CoinType -> ByteString -> Integer -> Redis (Either Reply Integer)
        getAction Incr = incrby
        getAction Decr = decrby

saveCoin' :: String -> Coin -> Redis (Maybe Integer)
saveCoin' name coin = do
  if getCoinScore coin < 1 then return Nothing
  else do
    lastid <- incrby lastkey 1
    case lastid of
      Left _ -> return Nothing
      Right id -> do
        set (pack . (++) prekey $ show id) coindata
        return (Just id)

  where prekey = "coins:" ++ name ++ ":"
        lastkey = pack $ prekey ++ "lastid"
        coindata = toStrict $ encode coin

joinCoinId :: String -> Integer -> ByteString
joinCoinId name id = pack $ "coins:" ++ name ++ ":" ++ idstr
  where idstr = show id

extractCoin :: Maybe ByteString -> Maybe Coin
extractCoin (Just v) = decodeStrict v
extractCoin Nothing  = Nothing

getCoins :: String -> Integer -> Integer -> Redis (Integer, [Coin])
getCoins name from size = do
  lastid <- getLastID name
  case lastid of
    Just id -> do
      let start = id - from
      let end = maximum [0, start - size]
      let idrange = reverse [end..start]
      let allkey = map (joinCoinId name) idrange
      result <- mget allkey
      case result of
        Right v -> do
          let ret = mapMaybe extractCoin v
          return (id, ret)
        _ -> return (0, [])
    Nothing -> return (0, [])

getLastID :: String -> Redis (Maybe Integer)
getLastID name = unpackScore <$> get lastidkey
  where lastidkey = pack $ "coins:" ++ name ++ ":lastid"

getScore :: String -> Redis (Maybe Score)
getScore name = unpackScore <$> get scorekey
  where scorekey = pack $ "coins:" ++ name ++ ":totalscore"

unpackScore::Either Reply (Maybe ByteString) -> Maybe Score
unpackScore (Right (Just v)) = Just (read $ unpack v :: Score)
unpackScore _                = Nothing

unpackIntegerScore::Either Reply Integer -> Maybe Score
unpackIntegerScore (Right v) = Just v
unpackIntegerScore _         = Nothing
