{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlRunner where

import System.Random
import Data.Text (Text, pack)
import Data.Scientific
import Database.PostgreSQL.Simple (Connection, query, Only, fromOnly)
import Thesis.Ast
import Thesis.LaplaceNoise (generate)
import Thesis.Microtransaction
import Thesis.Types
import Data.Time.Units (TimeUnit)

executeSql :: (TimeUnit t) => t -> StdGen -> Text -> Connection -> Select -> Epsilon -> IO (Text, StdGen)
executeSql timeout gen defaultAnswer conn ast e = do
  let (pquery, parameters) = selectToQuery ast
  queryResult <- (query conn pquery parameters) :: IO [Only Scientific]
  let sensitivity = getSensitivity (selectAggregation ast)
  let scale = e / sensitivity
  let (noise, newGen) = generate gen scale
  let computation = return $ pack $ show (fromOnly (head queryResult) + fromFloatDigits noise)
  transactionResult <- runMicrotransaction timeout defaultAnswer computation
  return (transactionResult, newGen)
