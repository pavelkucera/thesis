{-# LANGUAGE OverloadedStrings #-}

module Thesis.Parser where

import Data.Void (Void)
import Data.Text (Text, pack)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Text.Megaparsec (Parsec, (<|>), between, choice, empty, eof, many, manyTill, notFollowedBy, optional, parse, sepBy, try)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string')
import qualified Text.Megaparsec.Char.Lexer as L

import Thesis.Query.Ast

type Parser = Parsec Void String

parseQuery :: String -> Either (ParseErrorBundle String Void) Aggregation
parseQuery = parse query ""

query :: Parser Aggregation
query = between whitespace eof select

select :: Parser Aggregation
select = do
  _ <- word' "SELECT"
  agg <- aggregation
  expr <- parenthesized selectExpression
  from <- fromPart
  whereE <- optional wherePart
  _ <- optional (symbol ";")
  return $ agg (AggregationAst expr from whereE)
 where
  fromPart = word' "FROM" >> identifier
  wherePart = word' "WHERE" >> expression

aggregation :: Parser (AggregationAst -> Aggregation)
aggregation =
      database
  <|> stream
 where
  database = DatabaseAggregation <$> choice [
    Average <$ word' "AVG",
    Count <$ word' "COUNT",
    Sum <$ word' "SUM"
   ]
  stream = StreamAggregation <$> choice [
    Max <$ word' "MAX",
    Median <$ word' "MEDIAN",
    Min <$ word' "MIN"
   ]

selectExpression :: Parser Expr
selectExpression =
      star
  <|> expression
 where
  star = Star <$ lexeme (symbol "*")

expression :: Parser Expr
expression = makeExprParser term [
  [
    Prefix (PrefixOp NotOp <$ word' "NOT"),
    Postfix (PostfixOp (IsNotOp Nothing) <$ word' "IS NOT NULL"),
    Postfix (PostfixOp (IsOp Nothing) <$ word' "IS NULL"),
    Postfix (PostfixOp (IsNotOp $ Just True) <$ word' "IS NOT TRUE"),
    Postfix (PostfixOp (IsOp $ Just True) <$ word' "IS TRUE"),
    Postfix (PostfixOp (IsNotOp $ Just False) <$ word' "IS NOT FALSE"),
    Postfix (PostfixOp (IsOp $ Just False) <$ word' "IS FALSE")
  ],
  [
    InfixL (BinaryOp MultiplyOp <$ symbol "*"),
    InfixL (BinaryOp DivideOp <$ symbol "/")
  ],
  [
    InfixL (BinaryOp AddOp <$ symbol "+"),
    InfixL (BinaryOp SubtractOp <$ symbol "-")
  ],
  [
    InfixN (BinaryOp EqualOp <$ symbol "="),
    InfixN (BinaryOp GreaterOrEqualOp <$ symbol ">="),
    InfixN (BinaryOp GreaterOp <$ symbol ">"),
    InfixN (BinaryOp LessOp <$ symbol "<="),
    InfixN (BinaryOp LessOrEqualOp <$ symbol "<")
  ],
  [InfixL (BinaryOp AndOp <$ word' "AND")],
  [InfixL (BinaryOp OrOp <$ word' "OR")]
 ]

term :: Parser Expr
term =
      parenthesized expression
  <|> valueExpression
  <|> constantExpression

 -- | Supported value expression parser. Consumes whitespace.
 -- https://www.postgresql.org/docs/current/sql-expressions.html
valueExpression :: Parser Expr
valueExpression =
      try truthValue
  <|> column
 where
  column :: Parser Expr
  column = Column <$> identifier

  truthValue :: Parser Expr
  truthValue = lexeme $ true <|> false <|> nullE
   where
    true = Literal (Value True) <$ word' "TRUE"
    false = Literal (Value False) <$ word' "FALSE"
    nullE = Null <$ word' "NULL"

-- | Parses both regular and delimited identifiers. Consumes whitespace.
-- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
identifier :: Parser Text
identifier = pack <$> lexeme (delimitedIdentifier <|> regularIdentifier)
 where
  delimitedIdentifier :: Parser String
  delimitedIdentifier = char '\"' *> manyTill L.charLiteral (char '\"')

  regularIdentifier :: Parser String
  regularIdentifier = do
    first <- part
    rest <- optional $ symbol "." <> regularIdentifier
    return $ case rest of
      Nothing -> first
      Just r -> first ++ r
   where
    part :: Parser String
    part = do
      first <- firstChar
      rest <- many subsequentChar
      return $ first:rest
     where
      firstChar = choice [letterChar, char '_']
      subsequentChar = choice [alphaNumChar, char '_', char '$']

-- | String / number literals parser. Consumes following whitespace.
-- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-CONSTANTS
constantExpression :: Parser Expr
constantExpression =
      stringLiteral
  <|> numberLiteral
 where
  stringLiteral :: Parser Expr
  stringLiteral = Literal . Value <$> lexeme stringP
   where
    stringP = char '\'' *> manyTill L.charLiteral (char '\'')

  numberLiteral :: Parser Expr
  numberLiteral = Literal . Value <$> L.signed whitespace L.scientific

-- | Whitespace parser
whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "#") empty

-- | Case sensitive symbol parser
symbol :: String -> Parser String
symbol = L.symbol whitespace

-- | Case insensitive symbol parser
symbol' :: String -> Parser String
symbol' = L.symbol' whitespace

-- | Case insensitive word parser (word cannot be followed by any alpha numeric character)
word' :: String -> Parser ()
word' w = lexeme . try $ string' w *> notFollowedBy alphaNumChar

-- | Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

-- | Parses expression in parenthesis. There may be whitespace after each parenthesis.
parenthesized :: Parser a -> Parser a
parenthesized = between (symbol "(") (symbol ")")

-- | Parses expressions separated by commas. There may be whitespace after each comma.
commaList :: Parser a -> Parser [a]
commaList p = sepBy p (symbol ",")
