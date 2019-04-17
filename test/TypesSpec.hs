module TypesSpec (spec) where

import Types
import Data.Either (isRight)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "types" $ do
  describe "epsilon" $ do
      prop "returns an Epsilon when the input is positive" $
        \(Positive e) -> isRight (epsilon e)

      it "returns an error when the input is not positive" $ property $
        \(NonNegative e) -> (epsilon (-1 * e)) `shouldBe` Left (NonPositiveEpsilon (-1 * e))

  describe "delta" $ do
      prop "returns a Delta when the input is not negative" $
        \(NonNegative d) -> isRight (delta d)

      it "returns an error when the input is negative" $ property $
        \(Positive d) -> (delta (-1 * d)) `shouldBe` Left (NegativeDelta (-1 * d))
