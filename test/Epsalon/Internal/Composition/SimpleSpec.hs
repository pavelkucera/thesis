{-# LANGUAGE ScopedTypeVariables #-}

module Epsalon.Internal.Composition.SimpleSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Epsalon.Internal.Composition.PrivacyFilter
import Epsalon.Internal.Composition.Simple
import Epsalon.Internal.Types
import Epsalon.Internal.ValueGuard
import Epsalon.Internal.Composition.Helpers

spec :: Spec
spec = do
  it "returns an error when epsilon exceeds the budget" $
    let budget = (NonNegative 1, NonNegative 2)
        price = (Positive 2, NonNegative 1)
        state = emptyState budget :: SimpleCompositionState
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "returns an error when delta exceeds the budget" $
    let budget = (NonNegative 2, NonNegative 1)
        price = (Positive 1, NonNegative 2)
        state = emptyState budget :: SimpleCompositionState
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "returns updated budget" $
    let state = emptyState (NonNegative 2, NonNegative 2)
        price = (Positive 1, NonNegative 1)
    in subtractBudget state price `shouldBe` Right (SimpleCompositionState (NonNegative 1) (NonNegative 1))

  it "allows for a series of queries" $
    let state = emptyState (NonNegative 0.5, NonNegative (2**(-30))) :: SimpleCompositionState
        price = (Positive (2**(-11)), NonNegative 0)
    in countQueries state price `shouldBe` 1024

  prop "returns a smaller budget for a random epsilon" $
    \(QC.Positive (epsilon1 :: Epsilon)) (QC.Positive (epsilon2 :: Epsilon)) ->
      let initialEpsilon = max epsilon1 epsilon2
          queryEpsilon = min epsilon1 epsilon2
          budget = (NonNegative initialEpsilon, NonNegative 1)
          price = (Positive queryEpsilon, NonNegative 1)
          state = emptyState budget
          newBudget = subtractBudget state price
          Right (SimpleCompositionState newEpsilon _) = newBudget
      in value newEpsilon < initialEpsilon

  prop "returns a smaller budget for a random epsilon" $
    \(QC.Positive (delta1 :: Delta)) (QC.Positive (delta2 :: Delta)) ->
      let initialDelta = max delta1 delta2
          queryDelta = min delta1 delta2
          budget = (NonNegative 1, NonNegative initialDelta)
          price = (Positive 1, NonNegative queryDelta)
          state = emptyState budget
          newState = subtractBudget state price
          Right (SimpleCompositionState _ newDelta) = newState
      in value newDelta < initialDelta
