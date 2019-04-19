module Thesis.QueryRunner where

import Thesis.Microtransaction (runMicrotransaction)
import Thesis.LaplaceNoise (generate)
import Thesis.Query
import Thesis.SqlRunner (runSql)
import System.Random (getStdGen)

runQuery :: Query -> IO Text
runQuery query = do
  let queryResult = runMicrotransaction timeout (defaultAnswer query) (runSql query)
  let gen = getStdGen
  let noise = generate gen (getSensitivity query / getEpsilon query)
  return queryResult + noise
