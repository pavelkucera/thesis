module Thesis.QueryRunner where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask)
import Data.Scientific
import Data.Text (Text, pack)
import Data.Time.Units (TimeUnit)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Ast
import Thesis.LaplaceNoise (generate)
import Thesis.Mechanism (laplace, exponential)
import Thesis.Microtransaction (runMicrotransaction)
import Thesis.PrivacyFilter
import Thesis.Query
import Thesis.SqlGenerator (generateSql)
import Thesis.SqlRunner (executeSql)
import Thesis.ValueGuard

run :: (TimeUnit t, Monad m, PrivacyFilter p) => StdGen -> Connection -> p -> Query -> t -> m (p, StdGen, Either String Double)
run gen filter timeout conn query =
  let queryPrice = undefined
  in case subtractBudget p queryPrice of
      Left err -> return (filter, gen, Left err)
      Right newState ->
        let mechanism = laplace
        in do
          (queryResult, newGen) <- mechanism timeout gen myDefaultAnswer conn (ast query) epsilon
          return (newstate, newGen, queryResult)
