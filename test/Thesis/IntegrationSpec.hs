{-# LANGUAGE OverloadedStrings #-}

module Thesis.IntegrationSpec (spec) where

import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)
import System.Random
import Test.Hspec

import Thesis

seed :: Int
seed = 1337

initState :: IO (Connection, StdGen)
initState = do
  envHost <- lookupEnv "POSTGRES_HOST"
  let host = fromMaybe "localhost" envHost
      gen = mkStdGen seed
  connection <- connectPostgreSQL . pack $ "host=" ++ host ++ " dbname=postgres user=postgres password=password"
  return (connection, gen)

destroyState :: (Connection, StdGen) -> IO ()
destroyState = close . fst

withConnection :: ((Connection, StdGen) -> IO ()) -> IO ()
withConnection = bracket initState destroyState

spec :: Spec
spec = do
  around withConnection $ describe "run" $ do
    let (qEpsilon, budgetEpsilon) =
          case (positive 1, nonNegative 1) of
            (Right e1, Right e2) -> (e1, e2)
        privacyFilter = SimpleCompositionState budgetEpsilon zero

    it "counts using the Laplace mechanism" $
      \(connection, gen) -> do
        (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT COUNT(income) FROM households"
        -- Real count is 9,
        -- With this seed the first generated number is 0.23132540861101902
        -- Transformed to laplace noise as 0.7707826838949735
        -- Final answer must be 9.7707826838949735
        output `shouldBe` Right 9.7707826838949735

    it "sums using the Laplace mechanism" $
      \(connection, gen) -> do
        (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT SUM(income) FROM households"
        -- Real sum is 5.4
        -- With this seed the first generated number is 0.23132540861101902
        -- Transformed to laplace noise as 0.7707826838949735
        -- Final answer must be 6.1707826838949735
        output `shouldBe` Right 6.1707826838949735

    it "calculates averages using the Laplace mechanism" $
      \(connection, gen) -> do
        (_, _, output) <- runString gen connection privacyFilter qEpsilon "SELECT AVG(income) FROM households"
        -- Real average is 0.6
        -- With this seed the first generated number is 0.23132540861101902
        -- Transformed to laplace noise as 0.7707826838949735
        -- Final answer must be 1.3707826838949735
        output `shouldBe` Right 1.3707826838949735
