module Thesis.QueryRunner where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Composition.PrivacyFilter
import Thesis.Mechanism.Exponential (exponential)
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
    Right newState -> do
      (newGen, res) <- case query of
        DQuery q -> laplace gen conn q
        SQuery q -> exponential gen conn q
      return (newState, newGen, Right res)
