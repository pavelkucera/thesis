module Thesis.Mechanism.Laplace where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Ast
import Thesis.LaplaceNoise
import Thesis.Query
import Thesis.SqlGenerator (emitLaplace)
import Thesis.SqlRunner (executeSql)
import Thesis.ValueGuard

-- | Runs a DatabaseQuery using the Laplace mechanism
laplace :: (MonadIO m)
        => StdGen
        -> Connection
        -> DatabaseQuery
        -> m (StdGen, Double)
laplace gen connection (DatabaseQuery e ast@(DatabaseSelect (agg, _) _ _)) =
  let sql = emitLaplace ast
      sensitivity = getSensitivity agg
      noiseScale = value e / sensitivity
      (noise, newGen) = generate gen noiseScale
  in do
    trueAnswer <- executeSql connection sql
    return (newGen, trueAnswer + noise)
