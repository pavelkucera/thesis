module Thesis.QueryRunner where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Data.Time.Units (TimeUnit)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Composition.PrivacyFilter
import Thesis.Mechanism (laplace)
import Thesis.Query
import Thesis.ValueGuard (zeroDelta)

run :: (TimeUnit t, MonadIO m, MonadMask m, PrivacyFilter p) => StdGen -> Connection -> p -> Query -> t -> m (p, StdGen, Either BudgetDepleted Double)
run gen conn privacyFilter query timeout =
  case subtractBudget privacyFilter (queryEpsilon query, zeroDelta) of
    Left err -> return (privacyFilter, gen, Left err)
    Right newState ->
      let mechanism = laplace
      in do
        (newGen, queryResult) <- mechanism gen conn query timeout
        return (newState, newGen, queryResult)
