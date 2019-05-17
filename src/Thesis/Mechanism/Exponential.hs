module Thesis.Mechanism.Exponential (exponential) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Scientific
import Database.PostgreSQL.Simple (Connection, fold, query, Only(..))
import System.Random (randomR, random, StdGen)

import Thesis.Ast
import Thesis.Query
import Thesis.SqlBuilder
import Thesis.SqlGenerator
import Thesis.Types (Epsilon)
import Thesis.ValueGuard (value)

-- | Runs a StreamQuery using the exponential mechanism
exponential :: (MonadIO m)
            => StdGen
            -> Connection
            -> StreamQuery
            -> m (StdGen, Double)
exponential gen conn (StreamQuery e ast) =
  let aggregation = fst $ selectAggregationS ast
      (sql, params) = toQuery $ emitExponential ast
  in do
    resultCount <- countResults conn ast
    state <- liftIO $ fold conn sql params (emptyState gen) $ foldFun aggregation (value e) resultCount
    return (gen' state, val state)

countResults :: (MonadIO m) => Connection -> StreamSelect -> m Double
countResults conn ast =
  let (countSql, countParams) = toQuery $ emitCount ast
  in do
    result <- liftIO $ query conn countSql countParams
    return $ case result of
      [Only (Just v)] -> toRealFloat v
      _ -> 0

score :: StreamAggregation -> (Double -> AggregationState -> Double)
score agg len state = case agg of
  Median -> negate $ abs ((len / 2) - fromIntegral (count state))
  Min -> fromIntegral $ count state
  Max -> negate $ fromIntegral $ count state

data AggregationState = AggregationState {
  key :: Double,
  val :: Double,
  count :: Integer,
  gen' :: StdGen
} deriving (Show)

emptyState :: StdGen -> AggregationState
emptyState gen = AggregationState {
    key = 0,
    val = 0,
    count = 0,
    gen' = gen
}

foldFun :: StreamAggregation
        -> Epsilon
        -> Double
        -> AggregationState
        -> Only (Maybe Scientific)
        -> IO AggregationState
foldFun agg e len state currentRow =
  let currentVal = extractValue currentRow
      (rand1, g1) = random (gen' state) :: (Double, StdGen)
      (rand2, g2) = randomR (val state, currentVal) g1 :: (Double, StdGen)
      k = rand1 ** (1 / ((currentVal - val state) * exp (e * score agg len state)))
  in return $ AggregationState {
    key = if k > key state then k else key state,
    val = if k > key state then rand2 else val state,
    count = count state + 1,
    gen' = g2
  }
 where
  extractValue :: Only (Maybe Scientific) -> Double
  extractValue r = case r of
    Only (Just v) -> toRealFloat v
    _ -> 0
