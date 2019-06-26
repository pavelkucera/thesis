{-# LANGUAGE OverloadedStrings #-}

module Epsalon.Internal.Sql.BuilderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (concat)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text()

import Epsalon.Internal.Sql.Builder

spec :: Spec
spec = do
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
