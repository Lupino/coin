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

import           Coin
import           Coin.GraphQL           (schema, schemaByUser)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (Value, decode, object, (.=))
import           Data.Aeson.Result      (List (..))
import qualified Data.ByteString.Lazy   as LB (empty)
import           Data.GraphQL           (graphql)
import           Data.Int               (Int64)
import           Data.UnixTime
import           Database.PSQL.Types    (From (..), HasPSQL, Size (..))
import           Network.HTTP.Types     (status204)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (body, json, param, raw, rescue, status)
import           Web.Scotty.Utils       (errBadRequest, ok, okListResult,
                                         safeParam)


getScoreHandler :: HasPSQL u => ActionH u w ()
getScoreHandler = do
  name <- param "name"
  score <- lift $ getScore name
  ok "score" score

getInfoHandler :: HasPSQL u => ActionH u w ()
getInfoHandler = do
  name  <- param "name"
  inf  <- lift $ getInfo name
  score <- lift $ getScore name
  json $ object [ "score" .= score, "info" .= inf, "name" .= name ]

setInfoHandler :: HasPSQL u => ActionH u w ()
setInfoHandler = do
  name  <- param "name"
  wb <- body
  case (decode wb :: Maybe Value) of
    Nothing -> errBadRequest "Invalid coin info"
    Just v -> do
      void $ lift $ setInfo name v
      status status204
      raw LB.empty

dropCoinHandler :: HasPSQL u => ActionH u w ()
dropCoinHandler = do
  name  <- param "name"
  lift $ dropCoin name
  status status204
  raw LB.empty

paramPage :: ActionH u w (From, Size)
paramPage = do
  from <- From <$> safeParam "from" 0
  size <- Size <$> safeParam "size" 10
  return (from, size)

getCoinListHandler :: HasPSQL u => ActionH u w ()
getCoinListHandler = do
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinListHandler LQ1
    Just t  -> coinListHandler (LQ2 t)

getCoinListWithNameSpaceHandler :: HasPSQL u => ActionH u w ()
getCoinListWithNameSpaceHandler = do
  namespace <- param "namespace"
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinListHandler (LQ3 namespace)
    Just t  -> coinListHandler (LQ4 t namespace)

coinListHandler :: HasPSQL u => (Name -> ListQuery) -> ActionH u w ()
coinListHandler lq = do
  (from, size) <- paramPage
  name <- param "name"

  ret <- lift $ getCoinList (lq name) from size
  total <- lift $ countCoin (lq name)
  okListResult "coins" List
    { getTotal  = total
    , getFrom   = unFrom from
    , getSize   = unSize size
    , getResult = ret
    }

getCoinHistoryHandler :: HasPSQL u => ActionH u w ()
getCoinHistoryHandler = do
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinHistoryHandler HQ0
    Just t  -> coinHistoryHandler (HQ3 t)

getCoinHistoryByNameSpaceHandler :: HasPSQL u => ActionH u w ()
getCoinHistoryByNameSpaceHandler = do
  namespace <- param "namespace"
  tp <- readType <$> param "type" `rescue` (\_ -> return (""::String))
  case tp of
    Nothing -> coinHistoryHandler (HQ2 namespace)
    Just t  -> coinHistoryHandler (HQ6 namespace t)

coinHistoryHandler :: HasPSQL u => (Int64 -> Int64 -> HistQuery) -> ActionH u w ()
coinHistoryHandler hq = do
  (from, size) <- paramPage
  startTime <- param "start_time" `rescue` (\_ -> return 0)
  endTime <- param "end_time" `rescue` (\_ -> liftIO $ read . show . toEpochTime <$> getUnixTime)

  ret <- lift $ getHistories (hq startTime endTime) from size
  total <- lift $ countHistory (hq startTime endTime)
  okListResult "coins" List
    { getTotal  = total
    , getFrom   = unFrom from
    , getSize   = unSize size
    , getResult = ret
    }

saveCoinHandler :: HasPSQL u => ActionH u w ()
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

graphqlHandler :: HasPSQL u => ActionH u w ()
graphqlHandler = do
  query <- param "query"
  ret <- lift $ graphql schema query
  json ret

graphqlByUserHandler :: HasPSQL u => ActionH u w ()
graphqlByUserHandler = do
  query <- param "query"
  name  <- param "name"
  ret <- lift $ graphql (schemaByUser name) query
  json ret
