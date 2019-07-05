{-# LANGUAGE FlexibleInstances #-}

module Epsalon.Internal.Query.Runner where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Epsalon.Internal.Composition.PrivacyFilter
import Epsalon.Internal.Mechanism.Exponential
import Epsalon.Internal.Mechanism.Laplace
import Epsalon.Internal.Query.Ast
import Epsalon.Internal.ValueGuard

-- | Runs a query against a database and returns a differentially private result.
run :: (MonadIO m, PrivacyFilter p)
    => StdGen                                      -- ^ Source of randomness
    -> Connection                                  -- ^ Database connection
    -> p                                           -- ^ Privacy filter status, determines budget accounting.
    -> Query                                       -- ^ Query to run
    -> m (p, StdGen, Either BudgetDepleted Double) -- ^ Result, including updated privacy budget and source of randomness
run gen conn privacyFilter (Query e aggregation) =
  let (mechanism, delta) =
        case aggregation of
          DatabaseAggregation agg ast -> (laplace gen conn e agg ast, zero)
          StreamAggregation agg ast -> (exponential gen conn e agg ast, zero)
  in case subtractBudget privacyFilter (e, delta) of
    Left err -> return (privacyFilter, gen, Left err)
    Right newState -> do
      (newGen, res) <- mechanism
      return (newState, newGen, Right res)
