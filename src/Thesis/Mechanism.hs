module Thesis.Mechanism where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Mechanism.Laplace (laplace)
import Thesis.Mechanism.Exponential (exponential)
import Thesis.Query.Query
import Thesis.Query.Ast
import Thesis.ValueGuard
import Thesis.Types

class Mechanism a where
  perform :: (MonadIO m) => StdGen -> Connection -> Query a -> m (StdGen, Double)
  delta :: Query a -> NonNegative Delta

instance Mechanism DatabaseAggregation where
  perform = laplace
  delta _ = zero

instance Mechanism StreamAggregation where
  perform = exponential
  delta _ = zero
