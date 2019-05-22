{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveLift #-}

module Thesis.Query.Ast where

import Language.Haskell.TH.Syntax

import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Database.PostgreSQL.Simple.ToField (ToField)

type Identifier = Text
instance Lift Text where
  lift a = dataToExpQ (const Nothing) a

data Value where
  Value :: (Eq a, ToField a, Show a, Typeable a) => a -> Value
instance Lift Value where
  lift a = dataToExpQ (const Nothing) a
-- Fails because Value is not an instance of Data

instance Eq Value where
  (Value x) == (Value y) =
    cast x == Just y

instance Show Value where
  show (Value x) = show x

data Expr =
    Literal Value
  | Column Identifier
  | PrefixOp Identifier Expr
  | PostfixOp Identifier Expr
  | BinaryOp Identifier Expr Expr
  | FunctionCall Identifier [Expr]
  | Case (Expr, Expr) [(Expr, Expr)] (Maybe Expr)
  | Null
  | Star
  deriving (Eq, Show, Lift)

data AggregationAst =
  AggregationAst {
    selectExpr :: Expr,
    selectFrom :: Identifier,
    selectWhere :: Maybe Expr
  }
  deriving (Eq, Show, Lift)

data DatabaseAggregation =
   Sum
 | Average
 | Count
 deriving (Eq, Show, Lift)

data StreamAggregation =
   Median
 | Min
 | Max
 deriving (Eq, Show, Lift)

data Aggregation =
    DatabaseAggregation DatabaseAggregation AggregationAst
  | StreamAggregation StreamAggregation AggregationAst
  deriving (Eq, Show, Lift)
