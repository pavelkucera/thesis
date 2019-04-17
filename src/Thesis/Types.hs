module Thesis.Types (
  Epsilon,
  Delta,
  NonPositiveEpsilon(..),
  NegativeDelta(..),
  epsilon,
  getEpsilon,
  delta,
  getDelta
) where

newtype Epsilon = Epsilon Double
  deriving (Show, Eq, Ord)

newtype NonPositiveEpsilon = NonPositiveEpsilon Double
  deriving (Eq, Show)

newtype Delta = Delta Double
  deriving (Show, Eq, Ord)

newtype NegativeDelta = NegativeDelta Double
  deriving (Eq, Show)

epsilon :: Double -> Either NonPositiveEpsilon Epsilon
epsilon e | e > 0 = Right (Epsilon e)
          | otherwise = Left (NonPositiveEpsilon e)

getEpsilon :: Epsilon -> Double
getEpsilon (Epsilon e) = e

delta :: Double -> Either NegativeDelta Delta
delta d | d >= 0 = Right (Delta d)
        | otherwise = Left (NegativeDelta d)

getDelta :: Delta -> Double
getDelta (Delta d) = d
