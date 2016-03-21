{-# LANGUAGE OverloadedStrings #-}
module Lib ( saveCoin, getLastID, getCoins, getScore, Coin(..) ) where

import Database.Redis
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Control.Monad.Trans (liftIO)

data Coin a b c = Incr Integer Integer String | Decr Integer Integer String deriving (Show, Read)

-- user:{username}:coins:totalscore
-- user:{username}:coins:{coinId}
-- user:{username}:coins:lastid

saveCoin ::Connection -> String -> Coin a b c -> IO (Maybe Integer)
saveCoin conn name (Incr score time desc) = runRedis conn $ do
  lastid <- flip incrby 1 $ pack $ prekey ++ "lastid"
  case lastid of
    Right id -> do
      set (pack $ (++) prekey $ show id) $ pack coin
  totalscore <- flip incrby score $ pack $ prekey ++ "totalscore"
  case totalscore of
    Right v -> do
      return (Just v)
    _ -> do
      return Nothing
  where prekey = "user:" ++ name ++ ":coins:"
        coin = show (Incr score time desc)

saveCoin conn name (Decr score time desc) = runRedis conn $ do
  lastid <- flip incrby 1 $ pack $ prekey ++ "lastid"
  case lastid of
    Right id -> do
      set (pack $ (++) prekey $ show id) $ pack coin
  totalscore <- flip decrby score $ pack $ prekey ++ "totalscore"
  case totalscore of
    Right v -> do
      return (Just v)
    _ -> do
      return Nothing
  where prekey = "user:" ++ name ++ ":coins:"
        coin = show (Decr score time desc)

joinCoinIds :: String -> Integer -> ByteString
joinCoinIds name id = pack $ "user:" ++ name ++ ":coins:" ++ idstr
  where idstr = show id

extractCoin :: Maybe ByteString -> Maybe (Coin Integer Integer String)
extractCoin (Just v) = Just coin
  where coin = read $ unpack v :: Coin Integer Integer String
extractCoin Nothing = Nothing

getCoins :: Connection -> String -> Integer -> Integer -> IO (Maybe (Integer, [Maybe (Coin Integer Integer String)]))
getCoins conn name from size = do
  lastid <- getLastID conn name
  case lastid of
    Just id -> do
      let start = id - from
      let end = maximum([0, start - size])
      let idrange = reverse [end..start]
      let allkey = map (joinCoinIds name) idrange
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
  case lastid of
    Right (Just v) -> do
      return (Just (read $ unpack v :: Integer))
    _ -> do
      return Nothing
  where lastidkey = "user:" ++ name ++ ":coins:lastid"

getScore :: Connection -> String -> IO (Maybe Integer)
getScore conn name = runRedis conn $ do
  score <- get $ pack scorekey
  case score of
    Right (Just v) -> do
      return (Just (read $ unpack v :: Integer))
    _ -> do
      return Nothing
  where scorekey = "user:" ++ name ++ ":coins:totalscore"
