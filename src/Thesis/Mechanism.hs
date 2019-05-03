module Thesis.Mechanism (laplace, exponential) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Data.Time.Units (TimeUnit)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Ast
import Thesis.Composition.PrivacyFilter
import Thesis.LaplaceNoise (generate)
import Thesis.Microtransaction (runMicrotransaction)
import Thesis.Query
import Thesis.SqlGenerator (generateSql)
import Thesis.SqlRunner (executeSql)
import Thesis.ValueGuard

laplace :: (TimeUnit t, MonadIO m, MonadMask m) => StdGen -> Connection -> Query -> t -> m (StdGen, Either BudgetDepleted Double)
laplace gen conn query timeout =
  let sqlQuery = generateSql (queryAst query)
      (defaultAns, tempGen) = defaultAnswer gen query
      scale = value (queryEpsilon query) / getSensitivity (fst $ selectAggregation (queryAst query))
      (noise, newGen) = generate tempGen scale
      computation = executeSql conn sqlQuery
  in do
    transactionResult <- runMicrotransaction timeout defaultAns computation
    return (newGen, Right $ transactionResult + noise)

exponential :: (TimeUnit t, MonadIO m, MonadMask m) => StdGen -> Connection -> Query -> t -> m (StdGen, Either String Double)
exponential = undefined

defaultAnswer :: StdGen -> Query -> (Double, StdGen)
defaultAnswer gen _ = (1337, gen)
