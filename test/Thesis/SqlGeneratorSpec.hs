{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.SqlGeneratorSpec (spec) where

import Data.Text (Text, pack, append)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text

import Thesis.Ast
import Thesis.Query
import Thesis.SqlGenerator.Internal
import Thesis.SqlBuilder (SqlPart(..), Parameter(..))
import Thesis.ValueGuard

testExpr :: Expr
testExpr = (Literal (Value ("test" :: String)))

testParameter :: Parameter
testParameter = Parameter ("test" :: String)

spec :: Spec
spec =
  describe "Thesis.SqlGenerator" $ do
    describe "emitExpr" $ do
      prop "emits a parameter for literal values" $
        \(val :: String) ->
          emitExpr (Literal (Value val)) `shouldBe` SqlPart "?" [Parameter val]

      prop "emits a parameter for columns" $
        \(identifier :: String) ->
          emitExpr (Column $ pack identifier) `shouldBe` SqlPart "?" [Parameter identifier]

      prop "emits prefix operations correctly" $
        \(identifier :: Text) ->
          emitExpr (PrefixOp identifier testExpr) `shouldBe` SqlPart (identifier `append` " ?") [Parameter ("test" :: String)] 

      prop "emits postfix operations correctly" $
        \(identifier :: Text) ->
          emitExpr (PostfixOp identifier testExpr) `shouldBe` SqlPart ("? " `append` identifier) [Parameter ("test" :: String)] 

      prop "emits binary operations correctly" $
        \(identifier :: Text) ->
          emitExpr (BinaryOp testExpr identifier testExpr) `shouldBe` SqlPart ("? " `append` identifier `append` " ?") [testParameter, testParameter]

      prop "emits function calls correctly" $
          \(identifier :: Text) ->
            emitExpr (FunctionCall identifier [testExpr]) `shouldBe` SqlPart (identifier `append` "(?)") [testParameter]

    describe "emitAggregation" $ do
      it "emits average correctly" $
        emitAggregation (Average testExpr) `shouldBe` SqlPart " AVG(?)" [testParameter]

      it "emits sum correctly" $
        emitAggregation (Sum testExpr) `shouldBe` SqlPart " SUM(?)" [testParameter]

      it "emits count with an expression correctly" $
        emitAggregation (Count $ CountExpr testExpr) `shouldBe` SqlPart " COUNT(?)" [testParameter]

      it "emits count with a star correctly" $
        emitAggregation (Count $ Star) `shouldBe` SqlPart " COUNT(*)" []

    describe "emitWhere" $ do
      it "emits an existing where clause correctly" $
        emitWhere (Just testExpr) `shouldBe` SqlPart " WHERE ?" [testParameter]

      it "emits an empty where clause correctly" $
        emitWhere (Nothing) `shouldBe` mempty

    describe "emitFrom" $
      prop "emits a from clause correctly" $
        \(identifier :: Text) ->
          emitFrom identifier `shouldBe` SqlPart " FROM ?" [testParameter]

    describe "generateSql" $
      it "emits SQL based on a query" $
        case positive 1 of
          (Right e) ->
            generateSql (Query e $ Select (Count Star) "table" $ Just testExpr) `shouldBe`
            SqlPart "SELECT * FROM ? WHERE ?" [Parameter ("table" :: Text), testParameter]
          (Left err) -> expectationFailure $ show err
