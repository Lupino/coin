{-# LANGUAGE OverloadedStrings #-}

module Coin.Handler
  (
    getScoreHandler
  , getInfoHandler
  , setInfoHandler
  , getCoinListHandler
  , getCoinListWithNameSpaceHandler
  , saveCoinHandler
  , graphqlHandler
  , graphqlByUserHandler
  , getCoinHistoryHandler
  , getCoinHistoryByNameSpaceHandler
  , dropCoinHandler
  ) where

import           Coin.GraphQL            (schema, schemaByUser)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (lift)
import           Data.Aeson              (Value, decode, object, (.=))
import qualified Data.ByteString.Lazy    as LB (empty)
import           Data.GraphQL            (graphql)
import           Network.HTTP.Types      (status204)
import           Web.Scotty.Trans        (body, json, param, raw, rescue,
                                          status)

import           Coin
import           Data.UnixTime

import           Yuntan.Types.ListResult (ListResult (..))
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.Scotty     (errBadRequest, ok, okListResult)

import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl)
import           Yuntan.Types.HasMySQL   (HasMySQL)


getScoreHandler :: HasMySQL u => ActionH u ()
getScoreHandler = do
  name <- param "name"
  score <- lift $ getScore name
  ok "score" score

getInfoHandler :: HasMySQL u => ActionH u ()
getInfoHandler = do
  name  <- param "name"
  inf  <- lift $ getInfo name
  score <- lift $ getScore name
  json $ object [ "score" .= score, "info" .= inf, "name" .= name ]

setInfoHandler :: HasMySQL u => ActionH u ()
setInfoHandler = do
  name  <- param "name"
  wb <- body
  case (decode wb :: Maybe Value) of
    Nothing -> errBadRequest "Invalid coin info"
    Just v -> do
      lift $ setInfo name v
      status status204
      raw LB.empty

dropCoinHandler :: HasMySQL u => ActionH u ()
dropCoinHandler = do
  name  <- param "name"
  lift $ dropCoin name
  status status204
  raw LB.empty

paramPage :: ActionH u (From, Size)
paramPage = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  return (from, size)

getCoinListHandler :: HasMySQL u => ActionH u ()
getCoinListHandler = do
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinListHandler LQ1
    Just t  -> coinListHandler (LQ2 t)

getCoinListWithNameSpaceHandler :: HasMySQL u => ActionH u ()
getCoinListWithNameSpaceHandler = do
  namespace <- param "namespace"
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinListHandler (LQ3 namespace)
    Just t  -> coinListHandler (LQ4 t namespace)

coinListHandler :: HasMySQL u => (String -> ListQuery) -> ActionH u ()
coinListHandler lq = do
  (from, size) <- paramPage
  name <- param "name"

  ret <- lift $ getCoinList (lq name) from size
  total <- lift $ countCoin (lq name)
  okListResult "coins" ListResult { getTotal  = total
                                  , getFrom   = from
                                  , getSize   = size
                                  , getResult = ret
                                  }

getCoinHistoryHandler :: HasMySQL u => ActionH u ()
getCoinHistoryHandler = do
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinHistoryHandler HQ0
    Just t  -> coinHistoryHandler (HQ3 t)

getCoinHistoryByNameSpaceHandler :: HasMySQL u => ActionH u ()
getCoinHistoryByNameSpaceHandler = do
  namespace <- param "namespace"
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinHistoryHandler (HQ2 namespace)
    Just t  -> coinHistoryHandler (HQ6 namespace t)

coinHistoryHandler :: HasMySQL u => (Int64 -> Int64 -> HistQuery) -> ActionH u ()
coinHistoryHandler hq = do
  (from, size) <- paramPage
  startTime <- param "start_time" `rescue` (\_ -> return 0)
  endTime <- param "end_time" `rescue` (\_ -> liftIO $ read . show . toEpochTime <$> getUnixTime)

  ret <- lift $ getCoinHistory (hq startTime endTime) from size
  total <- lift $ countCoinHistory (hq startTime endTime)
  okListResult "coins" ListResult { getTotal  = total
                                  , getFrom   = from
                                  , getSize   = size
                                  , getResult = ret
                                  }

saveCoinHandler :: HasMySQL u => ActionH u ()
saveCoinHandler = do
  name  <- param "name"
  namespace <- param "namespace" `rescue` (\_ -> return "default")
  score <- param "score"
  desc  <- param "desc" `rescue` (\_ -> return "")
  tp    <- param "type"
  ct    <- param "created_at" `rescue` (\_ -> return 0)

  case readType tp of
    Just tp' -> do
      ret <- lift $ saveCoin namespace name (zeroCoin
        { getCoinScore = score
        , getCoinType = tp'
        , getCoinDesc = desc
        , getCoinCreatedAt = ct
        })


      ok "score" ret
    Nothing -> errBadRequest "Invalid type"

readType :: String -> Maybe CoinType
readType "Incr" = Just Incr
readType "Decr" = Just Decr
readType "incr" = Just Incr
readType "decr" = Just Decr
readType "INCR" = Just Incr
readType "DECR" = Just Decr
readType _      = Nothing

graphqlHandler :: HasMySQL u => ActionH u ()
graphqlHandler = do
  query <- param "query"
  ret <- lift $ graphql schema query
  json ret

graphqlByUserHandler :: HasMySQL u => ActionH u ()
graphqlByUserHandler = do
  query <- param "query"
  name  <- param "name"
  ret <- lift $ graphql (schemaByUser name) query
  json ret
