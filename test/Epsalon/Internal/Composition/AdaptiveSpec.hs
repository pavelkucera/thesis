{-# LANGUAGE ScopedTypeVariables #-}

module Epsalon.Internal.Composition.AdaptiveSpec (spec) where

import Test.Hspec
import Epsalon.Internal.Composition.PrivacyFilter
import Epsalon.Internal.Composition.Adaptive
import Epsalon.Internal.Composition.Helpers
import Epsalon.Internal.ValueGuard

spec :: Spec
spec = do
  it "returns an error when epsilon exceeds the budget" $
    let budget = (Positive 1, Positive 0.1)
        price = (Positive 1, NonNegative 0)
        state = emptyState budget :: AdaptiveCompositionState
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "returns an error when delta exceeds the budget" $
    let budget = (Positive 1, Positive 0.1)
        price = (Positive 1, NonNegative 0.1)
        state = emptyState budget :: AdaptiveCompositionState
    in subtractBudget state price `shouldBe` Left BudgetDepleted

  it "allows for a series of queries" $
    let budget = (Positive 0.5, Positive (2**(-30)))
        state = emptyState budget :: AdaptiveCompositionState
        price = (Positive (2**(-11)), NonNegative 0)
        count = countQueries state price
    in count `shouldBe` 10563
