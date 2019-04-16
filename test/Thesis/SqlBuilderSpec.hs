{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlBuilderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (concat, filter, length, pack)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text()
import Test.QuickCheck
import Thesis.SqlBuilder

instance Arbitrary Parameter where
  arbitrary = do
    value <- arbitrary :: Gen Text
    return $ Parameter value

emitPart :: Either ASCIIString Parameter -> SqlPart
emitPart value =
  case value of
    Left (ASCIIString text) -> emit $ T.pack text
    Right parameter -> emitParameter parameter

countQuestionMarks :: Text -> Int
countQuestionMarks = T.length . T.filter (== '?')

spec :: Spec
spec =
  describe "Thesis.SqlBuilder" $ do
    it "emits a parameter for an identifier" $
      emitIdentifier "name" `shouldBe` SqlPart "?" [Parameter $ Identifier "name"]

    it "emits a specific sql query" $
        emit "SELECT COUNT(*) FROM "
     <> emitIdentifier "table"
     <> emit " WHERE column = "
     <> emitParameter (5 :: Int)
        `shouldBe`
        SqlPart "SELECT COUNT(*) FROM ? WHERE column = ?" [Parameter $ Identifier "table" , Parameter (5 :: Int)]

    prop "emits text parts in correct order" $
      \(parts :: [Text]) ->
        let emitted = mconcat $ map emit parts
            expected = SqlPart (T.concat parts) mempty
        in emitted `shouldBe` expected

    prop "emits question marks corresponding to the number of parameters" $
      \(parts :: [Either (ASCIIString) Parameter]) ->
        let SqlPart sql ps = mconcat $ map emitPart parts
        in countQuestionMarks sql `shouldBe` length ps
