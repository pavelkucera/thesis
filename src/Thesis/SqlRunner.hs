{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlRunner where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Thesis.Ast
import Thesis.SqlBuilder

runSql :: Ast -> IO Text
runSql ast = do
  let sqlPart = emit ast
  executeSql "DB CONNECTION" sqlPart

executeSql :: ByteString -> SqlPart -> IO Text
executeSql connStr sql = do
  conn <- connectPostgreSQL connStr
  let (pquery, parameters) = toQuery sql
  [Only result] <- query conn pquery parameters
  return result
