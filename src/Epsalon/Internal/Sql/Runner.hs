module Epsalon.Internal.Sql.Runner where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Scientific
import Database.PostgreSQL.Simple (Connection, query, fold, Only(..), FromRow)

import Epsalon.Internal.Sql.Builder

-- | Helper to execute a numeric query. Turns NULL values into 0.
executeSql :: (MonadIO m) => Connection -> SqlPart -> m Double
executeSql conn sqlPart = do
  let (sqlQuery, sqlParameters) = toQuery sqlPart
  queryResult <- liftIO $ query conn sqlQuery sqlParameters
  return $ case queryResult of
    [Only (Just v)] -> toRealFloat v
    _ -> 0

-- | Helper to fold over a stream of values.
foldSql :: (MonadIO m, FromRow row)
        => Connection
        -> a
        -> (a -> row -> IO a)
        -> SqlPart
        -> m a
foldSql conn emptyState reducer sqlPart =
  let (sql, params) = toQuery sqlPart
  in liftIO $ fold conn sql params emptyState reducer

-- | Processes numeric results. Turns NULL values into 0.
extractValue :: Only (Maybe Scientific) -> Double
extractValue (Only (Just v)) = toRealFloat v
extractValue _ = 0
