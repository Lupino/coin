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

saveCoin ::Connection -> String -> Coin -> IO (Maybe Score)
saveCoin conn name coin = do
  lastid <- saveCoin' conn name coin
  case lastid of
    Just _  -> updateScore conn name coin
    Nothing -> return Nothing

updateScore :: Connection -> String -> Coin -> IO (Maybe Score)
updateScore conn name coin = runRedis conn $ unpackIntegerScore <$> getAction t scorekey score
  where scorekey = pack $ "coins:" ++ name ++ ":totalscore"
        score = getCoinScore coin
        t = getCoinType coin
        getAction :: CoinType -> ByteString -> Integer -> Redis (Either Reply Integer)
        getAction Incr = incrby
        getAction Decr = decrby

saveCoin' ::Connection -> String -> Coin -> IO (Maybe Integer)
saveCoin' conn name coin = do
  if getCoinScore coin < 1 then return Nothing
  else runRedis conn $ do
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

getCoins :: Connection -> String -> Integer -> Integer -> IO (Integer, [Coin])
getCoins conn name from size = do
  lastid <- getLastID conn name
  case lastid of
    Just id -> do
      let start = id - from
      let end = maximum [0, start - size]
      let idrange = reverse [end..start]
      let allkey = map (joinCoinId name) idrange
      runRedis conn $ do
        result <- mget allkey
        case result of
          Right v -> do
            let ret = mapMaybe extractCoin v
            return (id, ret)
          _ -> return (0, [])
    Nothing -> return (0, [])

getLastID :: Connection -> String -> IO (Maybe Integer)
getLastID conn name = runRedis conn $ do
  lastid <- get $ pack lastidkey
  return $ unpackScore lastid
  where lastidkey = "coins:" ++ name ++ ":lastid"

getScore :: Connection -> String -> IO (Maybe Score)
getScore conn name = runRedis conn $ (unpackScore <$> get (pack scorekey))
  where scorekey = "coins:" ++ name ++ ":totalscore"

unpackScore::Either Reply (Maybe ByteString) -> Maybe Score
unpackScore (Right (Just v)) = Just (read $ unpack v :: Score)
unpackScore _                = Nothing

unpackIntegerScore::Either Reply Integer -> Maybe Score
unpackIntegerScore (Right v) = Just v
unpackIntegerScore _         = Nothing
