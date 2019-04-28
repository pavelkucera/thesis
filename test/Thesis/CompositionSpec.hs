{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.CompositionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Thesis.Composition
import Thesis.Types
import Thesis.ValueGuard

budget :: Epsilon -> Delta -> (NonNegative Epsilon, NonNegative Delta)
budget epsilonValue deltaValue =
  case (nonNegative epsilonValue, nonNegative deltaValue) of
    (Right epsilon, Right delta) -> (epsilon, delta)
    _ -> error "Invalid budget"

queryBudget :: Epsilon -> Delta -> (Positive Epsilon, NonNegative Delta)
queryBudget epsilonValue deltaValue =
  case (positive epsilonValue, nonNegative deltaValue) of
    (Right epsilon, Right delta) -> (epsilon, delta)
    _ -> error "Invalid query price"

spec :: Spec
spec =
  describe "Composition" $
    describe "simpleSubtractBudget" $ do
      it "returns an error when epsilon exceeds the budget" $
        simpleSubtractBudget (budget 1 2) (queryBudget 2 2) `shouldBe` Left BudgetDepleted

      it "returns an error when delta exceeds the budget" $
        simpleSubtractBudget (budget 1 1) (queryBudget 1 2) `shouldBe` Left BudgetDepleted

      it "returns updated budget" $
        simpleSubtractBudget (budget 2 2) (queryBudget 1 1) `shouldBe` Right (budget 1 1)

      prop "returns a smaller budget for a random epsilon" $
        \(QC.Positive (epsilon1 :: Epsilon)) (QC.Positive (epsilon2 :: Epsilon)) ->
          let initialEpsilon = max epsilon1 epsilon2
              queryEpsilon = min epsilon1 epsilon2
              newBudget = simpleSubtractBudget (budget initialEpsilon 1) (queryBudget queryEpsilon 1)
              Right (newEpsilon, _) = newBudget
          in value newEpsilon < initialEpsilon

      prop "returns a smaller budget for a random delta" $
        \(QC.Positive (delta1 :: Delta)) (QC.Positive (delta2 :: Delta)) ->
          let initialDelta = max delta1 delta2
              queryDelta = min delta1 delta2
              Right (_, newDelta) = simpleSubtractBudget (budget 1 initialDelta) (queryBudget 1 queryDelta)
          in value newDelta < initialDelta

