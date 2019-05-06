module Thesis.Mechanism.Laplace where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)

import Thesis.Ast
import Thesis.LaplaceNoise (generate)
import Thesis.Query
import Thesis.SqlGenerator (generateSql)
import Thesis.SqlRunner (executeSql)
import Thesis.Transformer
import Thesis.ValueGuard

laplace :: (MonadIO m)
        => StdGen
        -> Connection
        -> Query
        -> m (StdGen, Double)
laplace gen connection query =
  let negatedQuery = negateQuery query
      sensitivity = getSensitivity (fst $ selectAggregation (queryAst query))
      noiseScale = value (queryEpsilon query) / sensitivity
      (noise, newGen) = generate gen noiseScale
  in do
    result <- execute connection query
    _ <- execute connection negatedQuery
    return (newGen, result + noise)

execute :: MonadIO m => Connection -> Query -> m Double
execute connection query =
  let sql = generateSql $ queryAst query
  in executeSql connection sql
