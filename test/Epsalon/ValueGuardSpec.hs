module Epsalon.ValueGuardSpec (spec) where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Epsalon.ValueGuard
import Epsalon.Types

spec :: Spec
spec = do
  describe "positive" $ do
    prop "returns the wrapped input when the input is positive" $
      \(QC.Positive e) -> isRight $ positive (e :: Epsilon)

    prop "returns an error when the input is not positive" $
      \(QC.NonNegative e) -> isLeft $ positive (-1 * e :: Epsilon)

  describe "nonNegative" $ do
    prop "returns the wrapped input when the input is not negative" $
      \(QC.NonNegative d) -> isRight $ nonNegative (d :: Epsilon)

    prop "returns an error when the input is negative" $
      \(QC.Positive d) -> isLeft $ nonNegative (-1 * d :: Epsilon)
