{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Epsalon.ParserSpec (spec) where

import Data.Scientific (Scientific)
import Text.Megaparsec (Parsec, ParseErrorBundle, Stream, eof, runParser, runParser')
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Megaparsec (initialState, shouldParse, succeedsLeaving)

import Epsalon.Parser
import Epsalon.Query.Ast

spec :: Spec
spec = do
  describe "constantExpression" $ do
    prop "parses random integers" $
      \(v :: Integer) -> parse constantExpression (show v) `shouldParse` (Literal $ Value (fromIntegral v :: Scientific))

    prop "parses random ints" $
      \(v :: Int) -> parse constantExpression (show v) `shouldParse` (Literal $ Value (fromIntegral v :: Scientific))

    it "parses an empty string" $
      parse constantExpression "''" `shouldParse` (Literal $ Value ("" :: String))

    it "parses a specific string" $
      parse constantExpression "'Jon\\'s claim.'" `shouldParse` (Literal $ Value ("Jon's claim." :: String))

  describe "valueExpression" $ do
    it "parses a delimited name" $
      parse valueExpression "\"column $ all of this 32š \"" `shouldParse` (Column "column $ all of this 32š ")

    it "parses a simple regular name" $
      parse valueExpression "column" `shouldParse` (Column "column")

    it "parses a regular name with two parts" $
      parse valueExpression "table.column" `shouldParse` (Column "table.column")

    it "parses a regular name with three parts" $
      parse valueExpression "schema.table.column" `shouldParse` (Column "schema.table.column")

    it "parses true" $
      parse valueExpression "true" `shouldParse` (Literal $ Value True)

    it "parses false" $
      parse valueExpression "false" `shouldParse` (Literal $ Value False)

    it "parses null" $
      parse valueExpression "null" `shouldParse` Null

  describe "expression" $ do
    it "parses an AND expresion" $
      parse expression "TRUE AND FALSE" `shouldParse` (BinaryOp AndOp (Literal $ Value True) (Literal $ Value False))

    it "parses an OR expresion" $
      parse expression "TRUE OR FALSE" `shouldParse` (BinaryOp OrOp (Literal $ Value True) (Literal $ Value False))

    it "parses a nested logical expression" $
      parse expression "TRUE AND FALSE OR TRUE" `shouldParse` (BinaryOp OrOp (BinaryOp AndOp (Literal $ Value True) (Literal $ Value False)) (Literal $ Value True))

    it "parses truth reserved words with priority" $
      runParser' expression (initialState "TRUE()") `succeedsLeaving` "()"

  describe "query" $ do
    it "parses an example query" $
      let expected = StreamAggregation Median (AggregationAst (Column "col") "people" (Just $ BinaryOp GreaterOp (Column "age") (Literal $ Value (20 :: Scientific))))
      in parse query "SELECT MEDIAN(col) FROM people WHERE age > 20" `shouldParse` expected

-- ^ Run a single parser and ensure that it consumes the whole string
parse :: (Ord e, Stream s) => Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser input = runParser (parser <* eof) "" input
