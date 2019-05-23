{-# LANGUAGE OverloadedStrings #-}

module Thesis.IntegrationSpec (spec) where

import Data.ByteString
import Database.PostgreSQL.Simple
import System.Random
import Test.Hspec

import Thesis.Composition.Simple
import Thesis.Query.Ast
import Thesis.Query.Query
import Thesis.Query.Runner (run)
import Thesis.ValueGuard (positive, nonNegative, zero)

seed :: Int
seed = 1337

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

spec :: Spec
spec = do
  describe "run" $ do
    let (qEpsilon, budgetEpsilon) = case (positive 1, nonNegative 1) of
                                      (Right e1, Right e2) -> (e1, e2)
        privacyFilter = SimpleCompositionState budgetEpsilon zero
    it "counts using the Laplace mechanism" $
      let countAst = DatabaseAggregation Count $ AggregationAst (Column "total_household_income") "households" Nothing
          countQuery = Query qEpsilon countAst
          gen = mkStdGen seed
      in do
        conn <- connectPostgreSQL connStr
        (_, _, output) <- run gen conn privacyFilter countQuery
        -- Real count is 41544,
        -- With this seed the first generated number is 0.23132540861101902
        -- Transformed to laplace noise as 0.7707826838949735
        -- Final answer must be 41544.7707826838949735
        output `shouldBe` Right 41544.7707826838949735

    it "sums using the Laplace mechanism" $
      let sumAst = DatabaseAggregation Sum $ AggregationAst (Column "total_household_income") "households" Nothing
          sumQuery = Query qEpsilon sumAst
          gen = mkStdGen seed
      in do
        conn <- connectPostgreSQL connStr
        (_, _, output) <- run gen conn privacyFilter sumQuery
        -- Real sum is -39803.23160805511989369339
        -- With this seed the first generated number is 0.23132540861101902
        -- Transformed to laplace noise as 0.7707826838949735
        -- Final answer must be -39802.46082537122
        output `shouldBe` Right (-39802.46082537122)

    it "calculates averages using the Laplace mechanism" $
      let avgAst = DatabaseAggregation Average $ AggregationAst (Column "total_household_income") "households" Nothing
          avgQuery = Query qEpsilon avgAst
          gen = mkStdGen seed
      in do
        conn <- connectPostgreSQL connStr
        (_, _, output) <- run gen conn privacyFilter avgQuery
        -- Real average is -0.95809819969321971629
        -- With this seed the first generated number is 0.23132540861101902
        -- Transformed to laplace noise as 0.7707826838949735
        -- Final answer must be -0.18731551579824623
        output `shouldBe` Right (-0.18731551579824623)
