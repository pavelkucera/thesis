module Types (Epsilon, Delta, epsilon, getEpsilon, delta, getDelta, QueryError(..)) where

newtype Epsilon = Epsilon Double
  deriving (Show, Eq, Ord)
newtype Delta = Delta Double
  deriving (Show, Eq, Ord)

epsilon :: Double -> Either QueryError Epsilon
epsilon e | e > 0   = Right (Epsilon e)
            | otherwise = Left (NonPositiveEpsilon e)

getEpsilon :: Epsilon -> Double
getEpsilon (Epsilon e) = e

delta :: Double -> Either QueryError Delta
delta d | d >= 0  = Right (Delta d)
          | otherwise = Left (NegativeDelta d)

getDelta :: Delta -> Double
getDelta (Delta d) = d

data QueryError = NonPositiveEpsilon Double
                | NegativeDelta Double
                | BudgetDepleted
  deriving (Show, Eq)
