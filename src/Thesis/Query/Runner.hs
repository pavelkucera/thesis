{-# LANGUAGE FlexibleInstances #-}

module Thesis.Query.Runner where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Composition.PrivacyFilter
import Thesis.Mechanism.Exponential
import Thesis.Mechanism.Laplace
import Thesis.Query.Ast
import Thesis.Query.Query
import Thesis.Types
import Thesis.ValueGuard

type Mechanism m agg = StdGen -> Connection -> Positive Epsilon -> agg -> AggregationAst -> m (StdGen, Double)

run :: (MonadIO m, PrivacyFilter p)
    => StdGen
    -> Connection
    -> p
    -> Query
    -> m (p, StdGen, Either BudgetDepleted Double)
run gen conn privacyFilter query@(Query e aggregation) =
  let (mechanism, delta) =
        case aggregation of
          DatabaseAggregation agg ast -> (laplace gen conn e agg ast, zero)
          StreamAggregation agg ast -> (exponential gen conn e agg ast, zero)
  in case subtractBudget privacyFilter (queryEpsilon query, delta) of
    Left err -> return (privacyFilter, gen, Left err)
    Right newState -> do
      (newGen, res) <- mechanism
      return (newState, newGen, Right res)
