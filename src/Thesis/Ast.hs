{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Ast where

import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.Types (Query(..))

import Thesis.SqlBuilder

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

selectToQuery :: Select -> (Query, [Parameter])
selectToQuery (Select sAgg sFrom sWhere) = toQuery $ emit "SELECT " <> emitAggregation sAgg <> emitFrom sFrom <> emitWhere sWhere
emitExpr :: Expr -> SqlPart
emitExpr (Literal (Value v)) = emit $ pack $ show v
emitExpr (Column identifier) = emitIdentifier identifier
emitExpr (PrefixOp identifier expr) = emit identifier <> emitExpr expr
emitExpr (PostfixOp identifier expr) = emitExpr expr <> emit identifier
emitExpr (BinaryOp expr1 identifier expr2) = emitExpr expr1 <> emit identifier <> emitExpr expr2
emitExpr (FunctionCall identifier exprs) = emit identifier <> emit "(" <> mconcat (emitExpr `map` exprs) <> emit ")"

emitAggregation :: Aggregation -> SqlPart
emitAggregation (Average expr) = emit "AVG(" <> emitExpr expr <> emit ") "
emitAggregation (Sum expr) = emit "SUM(" <> emitExpr expr <> emit ") "
emitAggregation (Count countExpr) = emit "COUNT(" <> case countExpr of
  Star -> emit "*"
  CountExpr expr -> emitExpr expr <> emit ") "
  <> emit ") "

emitWhere :: Maybe Expr -> SqlPart
emitWhere Nothing = emit ""
emitWhere (Just expr) = emit "WHERE " <> emitExpr expr

emitFrom :: Identifier -> SqlPart
emitFrom identifier = emit "FROM " <> emitIdentifier identifier <> emit " "
