{-# LANGUAGE GADTs #-}

module Epsalon.Mechanism.Exponential where

import Control.Monad.IO.Class (MonadIO)
import Data.Scientific (Scientific)
import Database.PostgreSQL.Simple (Connection, Only(..))
import System.Random (randomR, random, StdGen)

import Epsalon.Query.Ast
import Epsalon.Sql.Generator
import Epsalon.Sql.Runner
import Epsalon.Types (Epsilon)
import Epsalon.ValueGuard (Positive, value)

-- | Runs a query using the exponential mechanism. Values are streamed into and aggregated
-- in memory. Uses reservoir-sampling to avoid loading all values into memory at once.
exponential :: (MonadIO m)
            => StdGen
            -> Connection
            -> Positive Epsilon
            -> StreamAggregation
            -> AggregationAst
            -> m (StdGen, Double)
exponential gen conn e aggregation ast = do
    resultCount <- countResults conn ast
    result <- aggregate gen conn e aggregation ast resultCount
    return (stdGen result, val result)

-- Implementation of the A-Res reservoir sampling algorithm.
aggregate :: MonadIO m
          => StdGen
          -> Connection
          -> Positive Epsilon
          -> StreamAggregation
          -> AggregationAst
          -> Double
          -> m AggregationState
aggregate gen conn e aggregation ast resultCount =
  let aggregator = reducer aggregation resultCount
      sql = emitExponential aggregation ast
  in foldSql conn (emptyState gen) aggregator sql
 where
  reducer :: StreamAggregation
          -> Double
          -> AggregationState
          -> Only (Maybe Scientific)
          -> IO AggregationState
  reducer agg len state currentRow =
    let currentVal = extractValue currentRow
        (rand1, g1) = random (stdGen state) :: (Double, StdGen)
        (rand2, g2) = randomR (val state, currentVal) g1 :: (Double, StdGen)
        k = rand1 ** (1 / ((currentVal - val state) * exp (value e * score agg len state)))
    in return $ AggregationState {
      index = if k > index state then k else index state,
      val = if k > index state then rand2 else val state,
      count = count state + 1,
      stdGen = g2
    }

countResults :: (MonadIO m) => Connection -> AggregationAst -> m Double
countResults conn ast =
  let sql = emitCount ast
  in executeSql conn sql

score :: StreamAggregation -> Double -> AggregationState -> Double
score agg len state = case agg of
  Median{} -> negate $ abs ((len / 2) - fromIntegral (count state))
  Min{} -> fromIntegral $ count state
  Max{} -> negate $ fromIntegral $ count state

data AggregationState = AggregationState {
  index :: Double,
  val :: Double,
  count :: Integer,
  stdGen :: StdGen
} deriving (Show)

emptyState :: StdGen -> AggregationState
emptyState gen = AggregationState {
  index = 0,
  val = -1,
  count = 0,
  stdGen = gen
}
