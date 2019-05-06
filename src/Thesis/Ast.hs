{-# LANGUAGE GADTs #-}

module Thesis.Ast where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.ToField (ToField)

type Identifier = Text

data Value where
  Value :: (Eq a, ToField a, Show a, Typeable a) => a -> Value

instance Show Value where
  show (Value x) = show x

data Expr =
    Literal Value
  | Column Identifier
  | PrefixOp Identifier Expr
  | PostfixOp Identifier Expr
  | BinaryOp Expr Identifier Expr
  | FunctionCall Identifier [Expr]
  | Case (Expr, Expr) [(Expr, Expr)] (Maybe Expr)
  | Null
  | Star
  deriving Show

data Aggregation = Average | Sum | Count
  deriving Show

data Select = Select {
  selectAggregation :: (Aggregation, Expr),
  selectFrom :: Identifier,
  selectWhere :: Maybe Expr
} deriving Show

getSensitivity :: Aggregation -> Double
getSensitivity Average = 1
getSensitivity Sum = 1
getSensitivity Count = 1
