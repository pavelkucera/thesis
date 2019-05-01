module Thesis.Mechanism (laplace, exponential) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Data.Time.Units (TimeUnit)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Ast
import Thesis.LaplaceNoise (generate)
import Thesis.Microtransaction (runMicrotransaction)
import Thesis.Query
import Thesis.SqlGenerator (generateSql)
import Thesis.SqlRunner (executeSql)
import Thesis.ValueGuard

laplace :: (TimeUnit t, MonadIO m, MonadMask m) => StdGen -> Connection -> Query -> t -> m (StdGen, Either String Double)
laplace gen conn query timeout =
  let sqlQuery = generateSql query
      (defaultAns, tempGen) = defaultAnswer gen query
      (noise, newGen) = generate tempGen $ (value $ queryEpsilon query) / getSensitivity (selectAggregation (queryAst query))
  in do
    computation <- return $ executeSql conn sqlQuery
    transactionResult <- runMicrotransaction timeout defaultAns computation
    return $ (newGen, Right $ transactionResult + noise)

exponential :: (TimeUnit t, MonadIO m, MonadMask m) => StdGen -> Connection -> Query -> t -> m (StdGen, Either String Double)
exponential = undefined

defaultAnswer :: StdGen -> Query -> (Double, StdGen)
defaultAnswer = undefined
