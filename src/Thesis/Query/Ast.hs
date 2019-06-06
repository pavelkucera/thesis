{-# LANGUAGE GADTs #-}

module Thesis.Query.Ast where

import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Database.PostgreSQL.Simple.ToField (ToField)

type Identifier = Text

data Value where
  Value :: (Eq a, ToField a, Show a, Typeable a) => a -> Value

instance Eq Value where
  (Value x) == (Value y) =
    cast x == Just y

instance Show Value where
  show (Value x) = show x

data FunctionCall =
   GreatestFn Expr Expr
 | LeastFn Expr Expr
 deriving (Eq, Show)

data PrefixOp =
   NotOp
 deriving (Eq, Show)

data PostfixOp =
   IsOp (Maybe Bool)
 | IsNotOp (Maybe Bool)
 deriving (Eq, Show)

data BinaryOp =
   MultiplyOp
 | DivideOp
 | AddOp
 | SubtractOp
 | EqualOp
 | LessOp
 | LessOrEqualOp
 | GreaterOp
 | GreaterOrEqualOp
 | AndOp
 | OrOp
 deriving (Eq, Show)

data Expr =
    Literal Value
  | Column Identifier
  | PrefixOp PrefixOp Expr
  | PostfixOp PostfixOp Expr
  | BinaryOp BinaryOp Expr Expr
  | FunctionCall FunctionCall
  | Null
  | Star
  deriving (Eq, Show)

data AggregationAst =
  AggregationAst {
    selectExpr :: Expr,
    selectFrom :: Identifier,
    selectWhere :: Maybe Expr
  }
  deriving (Eq, Show)

data DatabaseAggregation =
   Sum
 | Average
 | Count
 deriving (Eq, Show)

data StreamAggregation =
   Median
 | Min
 | Max
 deriving (Eq, Show)

data Aggregation =
    DatabaseAggregation DatabaseAggregation AggregationAst
  | StreamAggregation StreamAggregation AggregationAst
  deriving (Eq, Show)
