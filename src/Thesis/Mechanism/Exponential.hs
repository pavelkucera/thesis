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
exponential gen conn (StreamQuery e ast@(StreamSelect (agg, _) _ _)) =
  let (countSql, countParams) = toQuery $ emitCount ast
      (sql, params) = toQuery $ emitExponential ast
  in do
    countRes <- liftIO $ query conn countSql countParams
    let len = case countRes of
                [Only (Just v)] -> toRealFloat v
                _ -> 0
    state <- liftIO $ fold conn sql params (emptyState gen) $ foldFun agg (value e) len
    return (gen' state, val state)

score :: StreamAggregation -> (Double -> State -> Double)
score agg len state = case agg of
  Median -> negate $ abs ((len / 2) - fromIntegral (count state))
  Min -> fromIntegral $ count state
  Max -> negate $ fromIntegral $ count state

data State = State {
  key :: Double,
  val :: Double,
  count :: Integer,
  gen' :: StdGen
} deriving (Show)

emptyState :: StdGen -> State
emptyState gen = State {
    key = 0,
    val = 0,
    count = 0,
    gen' = gen
}

extract :: Only (Maybe Scientific) -> Double
extract r = case r of
  Only (Just v) -> toRealFloat v
  _ -> 0

foldFun :: StreamAggregation -> Epsilon -> Double -> State -> Only (Maybe Scientific) -> IO State
foldFun agg e len state currentRow =
  let currentVal = extract currentRow
      (rand1, g') = random (gen' state) :: (Double, StdGen)
      (rand2, g2) = randomR (val state, currentVal) g' :: (Double, StdGen)
      k = rand1 ** (1 / ((currentVal - val state) * exp (e * score agg len state)))
  in return $ State {
    key = if k > key state then k else key state,
    val = if k > key state then rand2 else val state,
    count = count state + 1,
    gen' = g2
  }
