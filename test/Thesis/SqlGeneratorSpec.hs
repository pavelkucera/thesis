{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.SqlGeneratorSpec (spec) where

import Data.Text (Text)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text()

import Thesis.Ast
import Thesis.SqlGenerator
import Thesis.SqlBuilder (SqlPart(..), Parameter(..))

testExpr :: Expr
testExpr = (Literal (Value ("test" :: String)))

testParameter :: Parameter
testParameter = Parameter ("test" :: String)

spec :: Spec
spec = do
  describe "emitExpr" $ do
    prop "emits a parameter for literal values" $
      \(val :: String) ->
        emitExpr (Literal (Value val)) `shouldBe` SqlPart "?" [Parameter val]

    prop "emits an identifier for columns" $
      \(identifier :: Text) ->
        emitExpr (Column $ identifier) `shouldBe` SqlPart "?" [Parameter $ Identifier identifier]

    prop "emits prefix operations correctly" $
      \(identifier :: Text) ->
        emitExpr (PrefixOp identifier testExpr) `shouldBe` SqlPart (identifier <> "?") [Parameter ("test" :: String)]

    prop "emits postfix operations correctly" $
      \(identifier :: Text) ->
        emitExpr (PostfixOp identifier testExpr) `shouldBe` SqlPart ("?" <> identifier) [Parameter ("test" :: String)]

    prop "emits binary operations correctly" $
      \(identifier :: Text) ->
        emitExpr (BinaryOp testExpr identifier testExpr) `shouldBe` SqlPart ("?" <> identifier <> "?") [testParameter, testParameter]

    prop "emits function calls correctly" $
        \(identifier :: Text) ->
          emitExpr (FunctionCall identifier [testExpr]) `shouldBe` SqlPart (identifier <> "(?)") [testParameter]

  describe "emitAggregation" $ do
    it "emits average correctly" $
      emitAggregation (Average testExpr) `shouldBe` SqlPart "AVG(?)" [testParameter]

    it "emits sum correctly" $
      emitAggregation (Sum testExpr) `shouldBe` SqlPart "SUM(?)" [testParameter]

    it "emits count with an expression correctly" $
      emitAggregation (Count $ CountExpr testExpr) `shouldBe` SqlPart "COUNT(?)" [testParameter]

    it "emits count with a star correctly" $
      emitAggregation (Count $ Star) `shouldBe` SqlPart "COUNT(*)" []

  describe "emitWhere" $ do
    it "emits an existing where clause correctly" $
      emitWhere (Just testExpr) `shouldBe` SqlPart "WHERE ?" [testParameter]

    it "emits an empty where clause correctly" $
      emitWhere (Nothing) `shouldBe` mempty

  describe "emitFrom" $
    prop "emits a from clause correctly" $
      \(identifier :: Text) ->
        emitFrom  identifier `shouldBe` SqlPart "FROM ?" [Parameter $ Identifier identifier]

  describe "generateSql" $
    it "emits SQL based on a query" $
      generateSql (Select (Count Star) "table" $ Just testExpr)
      `shouldBe` SqlPart "SELECT COUNT(*) FROM ? WHERE ?" [Parameter $ Identifier "table", testParameter]

