{-# LANGUAGE ScopedTypeVariables #-}

module CompositionSpec (spec) where

import Composition
import Types
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "composition" $
  describe "simple composition" $
    describe "subtract budget" $ do
      it "returns an error when epsilon exceeds the budget" $
        case (epsilon 1, epsilon 2, delta 1) of
          (Left err, _, _) -> expectationFailure (show err)
          (_, Left err, _) -> expectationFailure (show err)
          (_, _, Left err) -> expectationFailure (show err)
          (Right e1, Right e2, Right d) ->
            subtractBudget (SimpleComposition e1 d) (e2, d)
            `shouldBe` Left BudgetDepleted

      it "returns an error when delta exceeds the budget" $
        case (epsilon 1, delta 1, delta 2) of
          (Left err, _, _) -> expectationFailure (show err)
          (_, Left err, _) -> expectationFailure (show err)
          (_, _, Left err) -> expectationFailure (show err)
          (Right e, Right d1, Right d2) ->
            subtractBudget (SimpleComposition e d1) (e, d2)
            `shouldBe` Left BudgetDepleted

      it "returns an updated budget for a specific value" $
        case (epsilon 2, delta 2, epsilon 1, delta 1) of
          (Left err, _, _, _) -> expectationFailure (show err)
          (_, Left err, _, _) -> expectationFailure (show err)
          (_, _, Left err, _) -> expectationFailure (show err)
          (_, _, _, Left err) -> expectationFailure (show err)
          (Right epsilon2, Right delta2, Right epsilon1, Right delta1) ->
            subtractBudget (SimpleComposition epsilon2 delta2) (epsilon1, delta1)
            `shouldBe` Right (getEpsilon epsilon1, getDelta delta1)

      it "returns a smaller budget for a random epsilon" $ property $
        \(Positive epsilon1) (Positive epsilon2) ->
          let maxE = max epsilon1 epsilon2
              minE = min epsilon1 epsilon2
          in case (epsilon minE, epsilon maxE, delta 1) of
            (Left err, _, _) -> expectationFailure (show err)
            (_, Left err, _) -> expectationFailure (show err)
            (_, _, Left err) -> expectationFailure (show err)
            (Right queryEpsilon, Right e, Right d) ->
              let budget = SimpleComposition e d
                  newBudget = subtractBudget budget (queryEpsilon, d)
              in case newBudget of
                Left err -> expectationFailure (show err)
                Right (newEpsilon, _) -> newEpsilon <= getEpsilon e `shouldBe` True

      it "returns a smaller budget for a random delta" $ property $
        \(Positive delta1) (Positive delta2) ->
          let maxD = max delta1 delta2
              minD = min delta1 delta2
          in case (delta minD, delta maxD, epsilon 1) of
            (Left err, _, _) -> expectationFailure (show err)
            (_, Left err, _) -> expectationFailure (show err)
            (_, _, Left err) -> expectationFailure (show err)
            (Right queryDelta, Right d, Right e) ->
              let budget = SimpleComposition e d
                  newBudget = subtractBudget budget (e, queryDelta)
              in case newBudget of
                Left err -> expectationFailure (show err)
                Right (_, newDelta) -> newDelta <= getDelta d `shouldBe` True
