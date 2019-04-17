module TypesSpec (spec) where

import Types
import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "types" $ do
  describe "epsilon" $ do
      prop "returns an Epsilon when the input is positive" $
        \(Positive e) -> isRight (epsilon e)

      prop "returns an error when the input is not positive" $
        \(NonNegative e) -> (epsilon (-1 * e)) `shouldBe` Left (NonPositiveEpsilon (-1 * e))

  describe "delta" $ do
      prop "returns a Delta when the input is not negative" $
        \(NonNegative d) -> isRight (delta d)

      prop "returns an error when the input is negative" $
        \(Positive d) -> (delta (-1 * d)) `shouldBe` Left (NegativeDelta (-1 * d))
