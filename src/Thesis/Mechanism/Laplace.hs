module Thesis.Mechanism.Laplace where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen, random)

import Thesis.Ast
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
laplace gen connection (DatabaseQuery e ast) =
  let sql = emitLaplace ast
      aggregation = fst $ selectAggregationD ast
      noiseScale = value e / sensitivity aggregation
      (noise, newGen) = generateNoise gen noiseScale
  in do
    trueAnswer <- executeSql connection sql
    return (newGen, trueAnswer + noise)

sensitivity :: DatabaseAggregation -> Double
sensitivity Average = 1
sensitivity Sum = 1
sensitivity Count = 1

-- | Generates a random variable drawn from a Laplace distribution with the given scale using the
-- given random number generator.
generateNoise :: StdGen -> Double -> (Double, StdGen)
generateNoise gen scale =
  let (rand, newGen) = random gen     -- value in [0, 1)
      flippedUniform = rand - 0.5     -- value in [-0.5, 0.5)
      uniform = negate flippedUniform -- value in (-0.5, 0.5]
      noise = uniformToLaplace scale 0 uniform
  in (noise, newGen)

-- | Transforms a random variable drawn from a uniform distribution in (-0.5, 0.5] into a variable
-- with distribution Laplace(mean, scale). This function contains no checks on the input variable.
uniformToLaplace :: Double -- ^ The scale of the Laplace distribution
                 -> Double -- ^ The mean of the Laplace distribution
                 -> Double -- ^ A random variable drawn uniformly from (-0.5, 0.5]
                 -> Double -- ^ A random value drawn from Laplace(mean, scale)
uniformToLaplace scale mean uniform = mean - scale * signum uniform * log(1 - 2 * abs uniform)