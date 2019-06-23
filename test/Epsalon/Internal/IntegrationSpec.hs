{-# LANGUAGE OverloadedStrings #-}

module Epsalon.Internal.IntegrationSpec (spec) where

import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Data.Either (fromRight, isRight)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)
import System.Random
import Test.Hspec

import Epsalon

seed :: Int
seed = 1337

initState :: IO (Connection, StdGen, Positive Epsilon, SimpleCompositionState)
initState = do
  envHost <- lookupEnv "POSTGRES_HOST"
  let host = fromMaybe "localhost" envHost
      gen = mkStdGen seed
      budgetEpsilon = fromRight (error "negative epsilon") $ nonNegative 1
      queryEpsilon = fromRight (error "non-positive epsilon") $ positive 1
  connection <- connectPostgreSQL . pack $ "host=" ++ host ++ " dbname=postgres user=postgres password=password"
  return (connection, gen, queryEpsilon, SimpleCompositionState budgetEpsilon zero)

destroyState :: (Connection, StdGen, Positive Epsilon, SimpleCompositionState) -> IO ()
destroyState (connection, _, _, _) = close connection

withConnection :: ((Connection, StdGen, Positive Epsilon, SimpleCompositionState) -> IO ()) -> IO ()
withConnection = bracket initState destroyState

spec :: Spec
spec = do
  around withConnection $ describe "run" $ do
    describe "Laplace" $ do
      it "counts" $
        \(connection, gen, qEpsilon, privacyFilter) -> do
          (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT COUNT(income) FROM households"
          -- Real count is 9,
          -- With this seed the first generated number is 0.23132540861101902
          -- Transformed to laplace noise as 0.7707826838949735
          -- Final answer must be 9.7707826838949735
          output `shouldBe` Right 9.7707826838949735

      it "sums" $
        \(connection, gen, qEpsilon, privacyFilter) -> do
          (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT SUM(income) FROM households"
          -- Real sum is 5.4
          -- With this seed the first generated number is 0.23132540861101902
          -- Transformed to laplace noise as 0.7707826838949735
          -- Final answer must be 6.1707826838949735
          output `shouldBe` Right 6.1707826838949735

      it "calculates average" $
        \(connection, gen, qEpsilon, privacyFilter) -> do
          (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT AVG(income) FROM households"
          -- Real average is 0.6
          -- With this seed the first generated number is 0.23132540861101902
          -- Transformed to laplace noise as 0.7707826838949735
          -- Final answer must be 1.3707826838949735
          output `shouldBe` Right 1.3707826838949735

    describe "exponential" $ do
      it "gives median" $
        \(connection, gen, qEpsilon, privacyFilter) -> do
          (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT MEDIAN(household_head_age) FROM households"
          output `shouldSatisfy` isRight

      it "gives min" $
        \(connection, gen, qEpsilon, privacyFilter) -> do
          (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT MIN(household_head_age) FROM households"
          output `shouldSatisfy` isRight

      it "gives max" $
        \(connection, gen, qEpsilon, privacyFilter) -> do
          (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT MAX(household_head_age) FROM households"
          output `shouldSatisfy` isRight
