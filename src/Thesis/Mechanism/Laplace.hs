module Thesis.Mechanism.Laplace where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Ast
import Thesis.LaplaceNoise (generate)
import Thesis.Query
import Thesis.SqlGenerator (generateSql)
import Thesis.SqlRunner (executeSql)
import Thesis.ValueGuard

laplace :: (MonadIO m)
        => StdGen
        -> Connection
        -> Query
        -> m (StdGen, Double)
laplace gen connection query =
  let ast = queryAst query
      sql = generateSql ast
      sensitivity = getSensitivity (fst $ selectAggregation ast)
      noiseScale = value (queryEpsilon query) / sensitivity
      (noise, newGen) = generate gen noiseScale
  in do
    trueAnswer <- executeSql connection sql
    return (newGen, trueAnswer + noise)
