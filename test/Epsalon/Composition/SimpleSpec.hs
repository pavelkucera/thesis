{-# LANGUAGE ScopedTypeVariables #-}

module Epsalon.Composition.SimpleSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Epsalon.Composition.PrivacyFilter
import Epsalon.Composition.Simple
import Epsalon.Types
import Epsalon.ValueGuard
import Epsalon.Composition.Helpers

spec :: Spec
spec = do
  it "returns an error when epsilon exceeds the budget" $
    let state = emptyState (mkBudget 1 2) :: SimpleCompositionState
        price = mkPrice 2 1
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "returns an error when delta exceeds the budget" $
    let state = emptyState (mkBudget 2 1) :: SimpleCompositionState
        price = mkPrice 1 2
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "returns updated budget" $
    let state = emptyState (mkBudget 2 2)
        price = mkPrice 1 1
        (epsilon, delta) = mkBudget 1 1
    in subtractBudget state price `shouldBe` Right (SimpleCompositionState epsilon delta)

  it "allows for a series of queries" $
    let state = emptyState (mkBudget 0.5 (2**(-30))) :: SimpleCompositionState
        price = mkPrice (2**(-11)) 0
    in countQueries state price `shouldBe` 1024

  prop "returns a smaller budget for a random epsilon" $
    \(QC.Positive (epsilon1 :: Epsilon)) (QC.Positive (epsilon2 :: Epsilon)) ->
      let initialEpsilon = max epsilon1 epsilon2
          queryEpsilon = min epsilon1 epsilon2
          budget = mkBudget initialEpsilon 1
          price = mkPrice queryEpsilon 1
          state = emptyState budget
          newBudget = subtractBudget state price
          Right (SimpleCompositionState newEpsilon _) = newBudget
      in value newEpsilon < initialEpsilon

  prop "returns a smaller budget for a random epsilon" $
    \(QC.Positive (delta1 :: Delta)) (QC.Positive (delta2 :: Delta)) ->
      let initialDelta = max delta1 delta2
          queryDelta = min delta1 delta2
          budget = mkBudget 1 initialDelta
          price = mkPrice 1 queryDelta
          state = emptyState budget
          newState = subtractBudget state price
          Right (SimpleCompositionState _ newDelta) = newState
      in value newDelta < initialDelta

mkBudget :: Epsilon -> Delta -> Budget SimpleCompositionState
mkBudget epsilon delta =
  case (nonNegative epsilon, nonNegative delta) of
    (Right epsilon, Right delta) -> (epsilon, delta)
    _ -> error "Budget does not correspond to the requirements"
