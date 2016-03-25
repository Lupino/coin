{-# LANGUAGE OverloadedStrings #-}
module Coin ( saveCoin, getLastID, getCoins, getScore, Coin(..) ) where

import Database.Redis
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Control.Monad.Trans (liftIO)

type Score = Integer
type Created = Integer
type Description = String

data Coin = Incr Score Created Description | Decr Score Created Description deriving (Show, Read)

-- coins:{username}:totalscore
-- coins:{username}:{coinId}
-- coins:{username}:lastid

saveCoin ::Connection -> String -> Coin -> IO (Maybe Integer)
saveCoin conn name coin = do
  lastid <- saveCoin' conn name coin
  case lastid of
    Just _ -> updateScore conn name coin
    Nothing -> return Nothing

updateScore :: Connection -> String -> Coin -> IO (Maybe Integer)
updateScore conn name (Incr score _ _) = runRedis conn $ do
  totalscore <- incrby scorekey score
  return $ unpackIntegerScore totalscore
  where scorekey = pack $ "coins:" ++ name ++ ":totalscore"

updateScore conn name (Decr score _ _) = runRedis conn $ do
  totalscore <- decrby scorekey score
  return $ unpackIntegerScore totalscore
  where scorekey = pack $ "coins:" ++ name ++ ":totalscore"

saveCoin' ::Connection -> String -> Coin -> IO (Maybe Integer)
saveCoin' conn name coin = runRedis conn $ do
  lastid <- incrby lastkey 1
  case lastid of
    Right id -> do
      set (pack . (++) prekey $ show id) coindata
      return (Just id)
    _ -> do
      return Nothing
  where prekey = "coins:" ++ name ++ ":"
        lastkey = pack $ prekey ++ "lastid"
        coindata = pack $ show coin

joinCoinId :: String -> Integer -> ByteString
joinCoinId name id = pack $ "coins:" ++ name ++ ":" ++ idstr
  where idstr = show id

extractCoin :: Maybe ByteString -> Maybe (Coin)
extractCoin (Just v) = Just coin
  where coin = read $ unpack v :: Coin
extractCoin Nothing = Nothing

getCoins :: Connection -> String -> Integer -> Integer -> IO (Maybe (Integer, [Maybe (Coin)]))
getCoins conn name from size = do
  lastid <- getLastID conn name
  case lastid of
    Just id -> do
      let start = id - from
      let end = maximum([0, start - size])
      let idrange = reverse [end..start]
      let allkey = map (joinCoinId name) idrange
      runRedis conn $ do
        result <- mget allkey
        case result of
          Right v -> do
            let ret = map extractCoin v
            return (Just (id, ret))
          _ -> do
            return Nothing
    Nothing -> do
      return Nothing

getLastID :: Connection -> String -> IO (Maybe Integer)
getLastID conn name = runRedis conn $ do
  lastid <- get $ pack lastidkey
  return $ unpackScore lastid
  where lastidkey = "coins:" ++ name ++ ":lastid"

getScore :: Connection -> String -> IO (Maybe Integer)
getScore conn name = runRedis conn $ do
  score <- get $ pack scorekey
  return $ unpackScore score
  where scorekey = "coins:" ++ name ++ ":totalscore"

unpackScore::Either Reply (Maybe ByteString) -> Maybe Integer
unpackScore (Right (Just v)) = Just (read $ unpack v :: Integer)
unpackScore _ = Nothing

unpackIntegerScore::Either Reply Integer -> Maybe Integer
unpackIntegerScore (Right v) = Just v
unpackIntegerScore _ = Nothing
