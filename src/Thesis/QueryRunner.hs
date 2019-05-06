module Thesis.QueryRunner where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Composition.PrivacyFilter
import Thesis.Mechanism.Laplace (laplace)
import Thesis.Query
import Thesis.ValueGuard (zero)

run :: (MonadIO m, PrivacyFilter p)
    => StdGen
    -> Connection
    -> p
    -> Query
    -> m (p, StdGen, Either BudgetDepleted Double)
run gen conn privacyFilter query =
  case subtractBudget privacyFilter (queryEpsilon query, zero) of
    Left err -> return (privacyFilter, gen, Left err)
    Right newState ->
      let mechanism = laplace
      in do
        (newGen, queryResult) <- mechanism gen conn query
        return (newState, newGen, Right queryResult)
