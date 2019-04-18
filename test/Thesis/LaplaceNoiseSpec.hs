{-# LANGUAGE OverloadedStrings #-}

module Thesis.LaplaceNoiseSpec (spec) where

import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck
import Thesis.LaplaceNoise

spec :: Spec
spec =
  describe "Thesis.LaplaceNoise" $
    describe "uniformToLaplace" $ do
      prop "always transforms 0 into 0" $
        \scale mean -> uniformToLaplace scale mean 0 `shouldBe` mean

      prop "generates noise" $
        \seed scale ->
          let (noise, _) = generate (mkStdGen seed) scale
          in abs noise `shouldSatisfy` (>= 0)
