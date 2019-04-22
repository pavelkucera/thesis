{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlRunner where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Thesis.Ast
import Thesis.SqlBuilder

sqlConnectionString :: ByteString
sqlConnectionString = "host=localhost dbname=postgres user=leendert password=leendert"

executeSql :: Text -> IO Text
executeSql ast = do
  let sqlPart = emit ast
  conn <- connectPostgreSQL sqlConnectionString
  let (pquery, parameters) = toQuery sqlPart
  [Only result] <- query conn pquery parameters
  return result
