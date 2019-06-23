{-# LANGUAGE GADTs #-}

module Epsalon.Query.Ast where

import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Database.PostgreSQL.Simple.ToField (ToField)

type Identifier = Text

-- | Wrapper around user-supplied parameters
data Value where
  Value :: (Eq a, ToField a, Show a, Typeable a) => a -> Value

instance Eq Value where
  (Value x) == (Value y) =
    cast x == Just y

instance Show Value where
  show (Value x) = show x

-- | Supported function calls
data FunctionCall =
   GreatestFn Expr Expr
 | LeastFn Expr Expr
 deriving (Eq, Show)

-- | Supported prefix operators
data PrefixOp =
   NotOp
 deriving (Eq, Show)

-- | Supported postfix operators
data PostfixOp =
   IsOp (Maybe Bool)
 | IsNotOp (Maybe Bool)
 deriving (Eq, Show)

-- | Supported binary operators
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

-- | Supported expressions
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

-- Aggregations computed using the Laplace mechanism
data DatabaseAggregation =
   Sum
 | Average
 | Count
 deriving (Eq, Show)

-- Aggregations computed using the exponential mechanism
data StreamAggregation =
   Median
 | Min
 | Max
 deriving (Eq, Show)

-- | Type to differentiate between queries offloaded to the database and queries processed in memory
data Aggregation =
    DatabaseAggregation DatabaseAggregation AggregationAst
  | StreamAggregation StreamAggregation AggregationAst
  deriving (Eq, Show)

data AggregationAst =
  AggregationAst {
    selectExpr :: Expr,
    selectFrom :: Identifier,
    selectWhere :: Maybe Expr
  }
  deriving (Eq, Show)
