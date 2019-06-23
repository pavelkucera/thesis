{-# LANGUAGE TypeFamilies #-}

module Epsalon.Internal.Composition.Adaptive where

import Data.Bifunctor (bimap)
import Epsalon.Internal.Composition.PrivacyFilter (PrivacyFilter(..), Budget, BudgetDepleted(..))
import Epsalon.Internal.ValueGuard (Positive, value)
import Epsalon.Internal.Types

data AdaptiveCompositionState =
  AdaptiveCompositionState {
    budgetG :: Budget AdaptiveCompositionState,
    expEpsilonSum :: Double,
    squaredEpsilonSum :: Double,
    deltaSum :: Double
  }
  deriving (Eq, Show)

instance PrivacyFilter AdaptiveCompositionState where
  type Budget AdaptiveCompositionState = (Positive Epsilon, Positive Delta)
  emptyState budget = AdaptiveCompositionState budget 0 0 0
  subtractBudget state price =
    let (epsilonG, deltaG) = bimap value value $ budgetG state
        (epsilonI, deltaI) = bimap value value price
        expEpsilonSum' = expEpsilonSum state
                       + epsilonI * ((exp epsilonI - 1) / 2)
        squaredEpsilonSum' = squaredEpsilonSum state
                           + epsilonI**2
        deltaSum' = deltaSum state + deltaI
        firstPart = expEpsilonSum'
        cc = 28.04 * log (1 / deltaG)
        a = squaredEpsilonSum' + (epsilonG**2 / cc)
        b = 1 + 0.5 * log ((cc * squaredEpsilonSum') / epsilonG**2 + 1)
        c = log (2 / deltaG)
        secondPart = sqrt $ 2 * a * b * c
        epsilon = firstPart + secondPart
        delta = 2 * deltaSum'
    in if delta <= deltaG && epsilon <= epsilonG
       then Right $ AdaptiveCompositionState (budgetG state) expEpsilonSum' squaredEpsilonSum' deltaSum'
       else Left BudgetDepleted
