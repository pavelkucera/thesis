module Thesis.QueryRunner where

import Data.Scientific
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)
import Data.Time.Units (TimeUnit)

import Thesis.Query
import Thesis.SqlRunner (executeSql)
import Thesis.ValueGuard

runQuery :: (TimeUnit t) => t -> Connection -> StdGen -> Query -> IO (Text, StdGen)
runQuery timeout conn gen query = do
  let myDefaultAnswer = pack $ show $ fromFloatDigits (defaultAnswer query)
  let epsilon = value $ getEpsilon query
  (queryResult, newGen) <- executeSql timeout gen myDefaultAnswer conn (ast query) epsilon
  return (pack $ show queryResult, newGen)
