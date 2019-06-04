{-# LANGUAGE FlexibleInstances #-}

module Thesis.Query.Runner where

import Control.Monad.IO.Class (MonadIO)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)
import Text.Megaparsec.Error (ParseErrorBundle)

import Thesis.Composition.PrivacyFilter
import Thesis.Mechanism.Exponential
import Thesis.Mechanism.Laplace
import Thesis.Parser (parseQuery)
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

runString :: (MonadIO m, PrivacyFilter p)
          => StdGen
          -> Connection
          -> p
          -> Positive Epsilon
          -> String
          -> m (p, StdGen, Either (Either BudgetDepleted (ParseErrorBundle String Void)) Double)
runString gen conn privacyFilter e queryString =
  case parseQuery queryString of
    Right aggregation -> do
      (newState, newGen, queryResult) <- run gen conn privacyFilter (Query e aggregation)
      case queryResult of
        Right res -> return (newState, newGen, Right res)
        Left err -> return (newState, newGen, Left (Left err))
    Left err -> return (privacyFilter, gen, Left (Right err))
