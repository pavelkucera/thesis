module Thesis.Composition where

import Thesis.ValueGuard
import Thesis.Types

data BudgetDepleted = BudgetDepleted
  deriving (Eq, Show)

type SimpleBudget = (NonNegative Epsilon, NonNegative Delta)

-- | Applies simple composition theorem on the given parameters
simpleSubtractBudget :: SimpleBudget                          -- ^ Initial budget
                     -> (Positive Epsilon, NonNegative Delta) -- ^ Price of the query
                     -> Either BudgetDepleted SimpleBudget    -- ^ Either 'BudgetDepleted', if the query exceeds the budget, or the remaining budget
simpleSubtractBudget (initialEpsilon, initialDelta) (queryEpsilon, queryDelta) =
  let newEpsilonValue = value initialEpsilon - value queryEpsilon
      newDeltaValue = value initialDelta - value queryDelta
  in case (nonNegative newEpsilonValue, nonNegative newDeltaValue) of
    (Right epsilon, Right delta) -> Right (epsilon, delta)
    _ -> Left BudgetDepleted
