module Thesis.SqlRunner (executeSql) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Scientific
import Database.PostgreSQL.Simple (Connection, query, fromOnly)

import Thesis.SqlBuilder

executeSql :: (MonadIO m) => Connection -> SqlPart -> m Double
executeSql conn sqlPart = do
  let (sqlQuery, sqlParameters) = toQuery sqlPart
  queryResult <- liftIO $ query conn sqlQuery sqlParameters
  return $ toRealFloat $ fromOnly $ head queryResult
