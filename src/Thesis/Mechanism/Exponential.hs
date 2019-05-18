{-# LANGUAGE GADTs #-}

module Thesis.Mechanism.Exponential where

import Control.Monad.IO.Class (MonadIO)
import Data.Scientific (Scientific)
import Database.PostgreSQL.Simple (Connection, Only(..))
import System.Random (randomR, random, StdGen)

import Thesis.Ast
import Thesis.Query
import Thesis.Sql.Generator
import Thesis.Sql.Runner
import Thesis.Types (Epsilon)
import Thesis.ValueGuard (Positive, value)

-- | Runs a query using the exponential mechanism
exponential :: (MonadIO m)
            => StdGen
            -> Connection
            -> Query StreamAggregation
            -> m (StdGen, Double)
exponential gen conn (Query e ast) = do
    resultCount <- countResults conn ast
    result <- aggregate gen conn e ast resultCount
    return (stdGen result, val result)

aggregate :: MonadIO m
          => StdGen
          -> Connection
          -> Positive Epsilon
          -> SelectAst StreamAggregation
          -> Double
          -> m AggregationState
aggregate gen conn e ast resultCount =
  let aggregation = selectAggregation ast
      aggregator = reducer aggregation resultCount
      sql = emitExponential ast
  in foldSql conn (emptyState gen) aggregator sql
 where
  reducer :: Aggregation StreamAggregation
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

countResults :: (MonadIO m) => Connection -> SelectAst StreamAggregation -> m Double
countResults conn ast =
  let sql = emitCount ast
  in executeSql conn sql

score :: Aggregation StreamAggregation -> Double -> AggregationState -> Double
score agg len state = case agg of
  Median -> negate $ abs ((len / 2) - fromIntegral (count state))
  Min -> fromIntegral $ count state
  Max -> negate $ fromIntegral $ count state

data AggregationState = AggregationState {
  index :: Double,
  val :: Double,
  count :: Integer,
  stdGen :: StdGen
} deriving (Show)

emptyState :: StdGen -> AggregationState
emptyState gen = AggregationState {
  index = 0,
  val = 0,
  count = 0,
  stdGen = gen
}
