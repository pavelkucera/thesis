{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.MicrotransactionSpec (spec) where

import Control.Timeout (delay)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Units (Microsecond, Second, fromMicroseconds)
import Test.Hspec
import Thesis.Microtransaction

microseconds :: Integer -> Microsecond
microseconds = fromMicroseconds

spec :: Spec
spec =
  describe "Thesis.Microtransaction" $ do
    describe "posixToTimeUnit" $ do
      it "converts a specific date" $
        let date = fromGregorian 1994 1 4
            timeOfDay = secondsToDiffTime (45 * 60)
            posixTime = utcTimeToPOSIXSeconds $ UTCTime date timeOfDay
        in posixToTimeUnit posixTime `shouldBe` (757644300 :: Second)

    describe "time" $ do
      it "returns elapsed time greater than zero" $ do
        (elapsedTime, value) <- time $ delay (microseconds 1)
        elapsedTime `shouldSatisfy` (> microseconds 0)
        value `shouldBe` ()

      it "returns action value" $ do
        (_ :: Microsecond, value) <- time $ pure (42 :: Int)
        value `shouldBe` 42

    describe "runMicrotransaction" $ do
      it "waits after a quick action" $
        let defaultAnswer = 42
            answer = return 5
            targetTime = microseconds 1000
        in do
        startTime <- getPOSIXTime
        result <- runMicrotransaction targetTime defaultAnswer answer
        endTime <- getPOSIXTime
        let elapsedTime = posixToTimeUnit $ endTime - startTime
        elapsedTime `shouldSatisfy` (>= targetTime)
        result `shouldBe` (5 :: Int)

      it "returns the default answer on a timeout" $
        let defaultAnswer = 42
            answer = delay (10 :: Second) >> return 5
            targetTime = microseconds 1
        in runMicrotransaction targetTime defaultAnswer answer
           `shouldReturn` (defaultAnswer :: Int)
