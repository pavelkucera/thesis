module Epsalon.Internal.Mechanism.LaplaceSpec (spec) where

import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck
import Epsalon.Internal.Mechanism.Laplace

spec :: Spec
spec = do
  describe "uniformToLaplace" $ do
    prop "always transforms 0 into 0" $
      \scale mean -> uniformToLaplace scale mean 0 `shouldBe` mean

  describe "generateNoise" $ do
    prop "generates noise" $
      \seed scale ->
        let (noise, _) = generateNoise (mkStdGen seed) scale
        in abs noise `shouldSatisfy` (>= 0)
