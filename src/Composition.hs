{-# LANGUAGE TypeFamilies #-}

module Composition where

import Types

data BudgetDepleted = BudgetDepleted
  deriving (Eq, Show)

type SimpleBudget = (Double, Double)

simpleSubtractBudget :: SimpleBudget -> (Epsilon, Delta) -> Either BudgetDepleted SimpleBudget
simpleSubtractBudget (initialEpsilon, initialDelta) (queryEpsilon, queryDelta) =
  let newEpsilon = initialEpsilon - getEpsilon queryEpsilon
      newDelta = initialDelta - getDelta queryDelta
  in case (nonNegative newEpsilon, nonNegative newDelta) of
    (True, True) -> Right (newEpsilon, newDelta)
    _ -> Left BudgetDepleted
   where
    nonNegative :: Double -> Bool
    nonNegative x = x >= 0
