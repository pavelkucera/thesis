{-# LANGUAGE TypeFamilies #-}

module Composition where

import Types

class Composition a where
  type QueryPrice a :: *
  subtractBudget :: a -> QueryPrice a -> Either QueryError (Double, Double)

data SimpleComposition =
  SimpleComposition Epsilon Delta
  deriving (Eq, Show)

instance Composition SimpleComposition where
  type QueryPrice SimpleComposition = (Epsilon, Delta)
  subtractBudget (SimpleComposition e _) (queryEpsilon, _) | queryEpsilon > e = Left BudgetDepleted
  subtractBudget (SimpleComposition _ d) (_, queryDelta) | queryDelta > d = Left BudgetDepleted
  subtractBudget (SimpleComposition e d) (queryEpsilon, queryDelta) = Right (getEpsilon e - getEpsilon queryEpsilon, getDelta d - getDelta queryDelta)
