{-# LANGUAGE GADTs #-}

module Thesis.Ast where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.ToField (ToField)

type Identifier = Text

data Value where
  Value :: (Eq a, ToField a, Show a, Typeable a) => a -> Value

data Expr =
    Literal Value
  | Column Identifier
  | PrefixOp Identifier Expr
  | PostfixOp Identifier Expr
  | BinaryOp Expr Identifier Expr
  | FunctionCall Identifier [Expr]

data CountExpr =
    Star
  | CountExpr Expr

data Aggregation where
  Average :: Expr -> Aggregation
  Sum :: Expr -> Aggregation
  Count :: CountExpr -> Aggregation

data Select = Select {
  selectAggregation :: Aggregation,
  selectFrom :: Identifier,
  selectWhere :: Maybe Expr
}

getSensitivity :: Aggregation -> Double
getSensitivity (Average _) = 1
getSensitivity (Sum _) = 1
getSensitivity (Count _) = 1
