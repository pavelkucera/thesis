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
    Prefix (PrefixOp "NOT" <$ word' "NOT"),
    Postfix (PostfixOp "IS NOT NULL" <$ word' "IS NOT NULL"),
    Postfix (PostfixOp "IS NULL" <$ word' "IS NULL"),
    Postfix (PostfixOp "IS NOT TRUE" <$ word' "IS NOT TRUE"),
    Postfix (PostfixOp "IS TRUE" <$ word' "IS TRUE"),
    Postfix (PostfixOp "IS NOT FALSE" <$ word' "IS NOT FALSE"),
    Postfix (PostfixOp "IS FALSE" <$ word' "IS FALSE")
   ],
  [InfixL (BinaryOp "*" <$ symbol' "*"), InfixL (BinaryOp "/" <$ symbol' "/")],
  [InfixL (BinaryOp "+" <$ symbol' "+"), InfixL (BinaryOp "-" <$ symbol' "-")],
  [
    InfixN (BinaryOp "=" <$ symbol' "="), InfixN (BinaryOp ">=" <$ symbol' ">="),
    InfixN (BinaryOp ">" <$ symbol' ">"), InfixN (BinaryOp "<=" <$ symbol' "<="),
    InfixN (BinaryOp "<" <$ symbol' "<")
  ],
  [InfixL (BinaryOp "AND" <$ word' "AND")],
  [InfixL (BinaryOp "OR" <$ word' "OR")]
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

-- | Parsers both regular and delimited identifiers. Consumes whitespace.
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
