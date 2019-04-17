module Thesis.TypesSpec (spec) where

import Thesis.Types
import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  describe "Types" $ do
    describe "epsilon" $ do
      prop "returns an Epsilon when the input is positive" $
        \(Positive e) -> isRight $ epsilon e

      prop "returns an error when the input is not positive" $
        \(NonNegative e) -> isLeft $ epsilon (-1 * e)

    describe "delta" $ do
      prop "returns a Delta when the input is not negative" $
        \(NonNegative d) -> isRight $ delta d

      prop "returns an error when the input is negative" $
        \(Positive d) -> isLeft $ delta (-1 * d)
