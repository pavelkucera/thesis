{-# LANGUAGE ScopedTypeVariables #-}

module Epsalon.Composition.AdaptiveSpec (spec) where

import Test.Hspec
import Epsalon.Composition.PrivacyFilter
import Epsalon.Composition.Adaptive
import Epsalon.Composition.Helpers
import Epsalon.Types
import Epsalon.ValueGuard

spec :: Spec
spec = do
  it "returns an error when epsilon exceeds the budget" $
    let budget = mkBudget 1 0.1
        price = mkPrice 1 0
        state = emptyState budget :: AdaptiveCompositionState
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "returns an error when delta exceeds the budget" $
    let budget = mkBudget 1 0.1
        price = mkPrice 0.1 1
        state = emptyState budget :: AdaptiveCompositionState
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "allows for a series of queries" $
    let state = emptyState (mkBudget 0.5 (2**(-30))) :: AdaptiveCompositionState
        price = mkPrice (2**(-11)) 0
        count = countQueries state price
    in count `shouldBe` 10563

mkBudget :: Epsilon -> Delta -> Budget AdaptiveCompositionState
mkBudget epsilon delta =
  case (positive epsilon, positive delta) of
    (Right epsilon, Right delta) -> (epsilon, delta)
    _ -> error "Budget does not correspond to the requirements"
