{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.CompositionSpec (spec) where

import Thesis.Composition
import Thesis.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

queryBudget :: (Double, Double) -> (Epsilon, Delta)
queryBudget (epsilonValue, deltaValue) =
  case (epsilon epsilonValue, delta deltaValue) of
    (Right e, Right d) -> (e, d)
    _ -> error "Invalid query price"

spec :: Spec
spec =
  describe "Composition" $
    describe "simpleSubtractBudget" $ do
      it "returns an error when epsilon exceeds the budget" $
        simpleSubtractBudget (1, 2) (queryBudget (2, 2)) `shouldBe` Left BudgetDepleted

      it "returns an error when delta exceeds the budget" $
        simpleSubtractBudget (1, 1) (queryBudget (1, 2)) `shouldBe` Left BudgetDepleted

      it "returns updated budget" $
        simpleSubtractBudget (2, 2) (queryBudget (1, 1)) `shouldBe` Right (1, 1)

      prop "returns a smaller budget for a random epsilon" $
        \(Positive epsilon1) (Positive epsilon2) ->
          let initialEpsilon = max epsilon1 epsilon2
              queryEpsilon = min epsilon1 epsilon2
              Right (newEpsilon, _) = simpleSubtractBudget (initialEpsilon, 1) (queryBudget (queryEpsilon, 1))
          in newEpsilon < initialEpsilon

      prop "returns a smaller budget for a random delta" $
        \(Positive delta1) (Positive delta2) ->
          let initialDelta = max delta1 delta2
              queryDelta = min delta1 delta2
              Right (_, newDelta) = simpleSubtractBudget (1, initialDelta) (queryBudget (1, queryDelta))
          in newDelta < initialDelta
