{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Coin
  (
    saveCoin
  , getCoins
  , getScore
  , Coin (..)
  , CoinType (..)
  , zeroCoin
  , Score
  , Created
  , Description
  ) where

import           Data.Aeson             (FromJSON (..), ToJSON (..),
                                         decodeStrict, encode, object,
                                         withObject, (.:), (.=))
import           Data.ByteString.Char8  (ByteString, pack, unpack)
import           Data.ByteString.Lazy   (toStrict)
import           Database.Redis         (Connection, Redis, Reply, decrby, get,
                                         incrby, mget, runRedis, set)

import           Control.Monad          (when)
import           Data.Maybe             (fromJust, isJust, mapMaybe)

import           Control.Monad.IO.Class (liftIO)
import           Data.UnixTime

type Score       = Integer
type Created     = Integer
type Description = String

data CoinType = Incr | Decr deriving (Show, Read)

data Coin = Coin { getCoinType      :: CoinType
                 , getCoinScore     :: Score
                 , getCoinPreScore  :: Score
                 , getCoinDesc      :: Description
                 , getCoinCreatedAt :: Created
                 }

zeroCoin :: Coin
zeroCoin = Coin { getCoinType = Incr
                , getCoinScore = 0
                , getCoinPreScore = 0
                , getCoinDesc = ""
                , getCoinCreatedAt = 0
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

saveCoin :: String -> Coin -> Redis Score
saveCoin name coin = do
  lastid <- saveCoin' name =<< prepareSaveCoin name coin
  case lastid of
    Just _  -> updateScore name coin
    Nothing -> return 0

updateScore :: String -> Coin -> Redis Score
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

prepareSaveCoin :: String -> Coin -> Redis Coin
prepareSaveCoin name coin = do
  preScore <- getScore name
  ct' <- if ct > 0 then return ct else liftIO $ read . show . toEpochTime <$> getUnixTime
  return coin { getCoinPreScore = preScore
              , getCoinCreatedAt = ct'
              }

  where ct = getCoinCreatedAt coin



joinCoinId :: String -> Integer -> ByteString
joinCoinId name id = pack $ "coins:" ++ name ++ ":" ++ idstr
  where idstr = show id

extractCoin :: Maybe ByteString -> Maybe Coin
extractCoin (Just v) = decodeStrict v
extractCoin Nothing  = Nothing

getCoins :: String -> Integer -> Integer -> Redis (Integer, [Coin])
getCoins name from size = do
  lastid <- getLastID name
  if lastid > 0 then do
    let start = lastid - from
    let end = maximum [0, start - size]
    let idrange = reverse [end..start]
    let allkey = map (joinCoinId name) idrange
    result <- mget allkey
    case result of
      Right v -> do
        let ret = mapMaybe extractCoin v
        return (lastid, ret)
      _ -> return (0, [])
  else return (0, [])

getLastID :: String -> Redis Integer
getLastID name = unpackScore <$> get lastidkey
  where lastidkey = pack $ "coins:" ++ name ++ ":lastid"

getScore :: String -> Redis Score
getScore name = unpackScore <$> get scorekey
  where scorekey = pack $ "coins:" ++ name ++ ":totalscore"

unpackScore::Either Reply (Maybe ByteString) -> Score
unpackScore (Right (Just v)) = read $ unpack v :: Score
unpackScore _                = 0

unpackIntegerScore::Either Reply Integer -> Score
unpackIntegerScore (Right v) = v
unpackIntegerScore _         = 0
