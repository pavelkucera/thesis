{-# LANGUAGE TypeFamilies #-}

module Epsalon.Composition.Simple where

import Epsalon.Composition.PrivacyFilter (PrivacyFilter(..), BudgetDepleted(..))
import Epsalon.ValueGuard (NonNegative, nonNegative, value)
import Epsalon.Types

data SimpleCompositionState =
  SimpleCompositionState (NonNegative Epsilon) (NonNegative Delta)
  deriving (Eq, Show)

instance PrivacyFilter SimpleCompositionState where
  type Budget SimpleCompositionState = (NonNegative Epsilon, NonNegative Delta)
  emptyState (epsilon, delta) = SimpleCompositionState epsilon delta
  subtractBudget (SimpleCompositionState epsilon delta) (epsilonI, deltaI) =
    let newEpsilonValue = value epsilon - value epsilonI
        newDeltaValue = value delta - value deltaI
    in case (nonNegative newEpsilonValue, nonNegative newDeltaValue) of
      (Right newEpsilon, Right newDelta) -> Right $ SimpleCompositionState newEpsilon newDelta
      _ -> Left BudgetDepleted
