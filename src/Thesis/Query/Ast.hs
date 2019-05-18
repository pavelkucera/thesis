{-# LANGUAGE GADTs #-}

module Thesis.Query.Ast where

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
  | BinaryOp Identifier Expr Expr
  | FunctionCall Identifier [Expr]
  | Case (Expr, Expr) [(Expr, Expr)] (Maybe Expr)
  | Null
  | Star
  deriving Show

data DatabaseAggregation
data StreamAggregation

data Aggregation a where
  Sum :: Aggregation DatabaseAggregation
  Average :: Aggregation DatabaseAggregation
  Count :: Aggregation DatabaseAggregation
  Median :: Aggregation StreamAggregation
  Min :: Aggregation StreamAggregation
  Max :: Aggregation StreamAggregation

data SelectAst a = SelectAst {
  selectAggregation :: Aggregation a,
  selectExpr :: Expr,
  selectFrom :: Identifier,
  selectWhere :: Maybe Expr
}
