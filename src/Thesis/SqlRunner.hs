module Thesis.SqlRunner (executeSql, executeSqlList) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Scientific
import Database.PostgreSQL.Simple (Connection, query, Only(..))

import Thesis.SqlBuilder

executeSql :: (MonadIO m) => Connection -> SqlPart -> m Double
executeSql conn sqlPart = do
  let (sqlQuery, sqlParameters) = toQuery sqlPart
  queryResult <- liftIO $ query conn sqlQuery sqlParameters
  return $ case queryResult of
    [Only (Just v)] -> toRealFloat v
    _ -> 0

executeSqlList :: (MonadIO m) => Connection -> SqlPart -> m [Double]
executeSqlList conn sqlPart = do
  let (sqlQuery, sqlParameters) = toQuery sqlPart
  queryResult <- liftIO $ query conn sqlQuery sqlParameters
  return $ extractValue `map` queryResult

extractValue :: Only (Maybe Scientific) -> Double
extractValue (Only (Just v)) = toRealFloat v
extractValue _ = 0
