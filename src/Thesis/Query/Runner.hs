{-# LANGUAGE FlexibleInstances #-}

module Thesis.Query.Runner where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Composition.PrivacyFilter
import Thesis.Mechanism
import Thesis.Query.Query

run :: (MonadIO m, PrivacyFilter p, Mechanism a)
    => StdGen
    -> Connection
    -> p
    -> Query a
    -> m (p, StdGen, Either BudgetDepleted Double)
run gen conn privacyFilter query =
  case subtractBudget privacyFilter (queryEpsilon query, delta query) of
    Left err -> return (privacyFilter, gen, Left err)
    Right newState -> do
      (newGen, res) <- perform gen conn query
      return (newState, newGen, Right res)
