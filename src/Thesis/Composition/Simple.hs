module Thesis.Composition.Simple where

import Thesis.Composition.PrivacyFilter (PrivacyFilter(..), BudgetDepleted(..))
import Thesis.ValueGuard (NonNegative, nonNegative, value)
import Thesis.Types

data SimpleCompositionState =
  SimpleCompositionState (NonNegative Epsilon) (NonNegative Delta)
  deriving (Eq, Show)

instance PrivacyFilter SimpleCompositionState where
  emptyState (epsilon, delta) = SimpleCompositionState epsilon delta
  subtractBudget (SimpleCompositionState epsilon delta) (epsilonI, deltaI) =
    let newEpsilonValue = value epsilon - value epsilonI
        newDeltaValue = value delta - value deltaI
    in case (nonNegative newEpsilonValue, nonNegative newDeltaValue) of
      (Right newEpsilon, Right newDelta) -> Right $ SimpleCompositionState newEpsilon newDelta
      _ -> Left BudgetDepleted
