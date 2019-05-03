{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlGenerator where

import Thesis.Ast
import Thesis.SqlBuilder

-- | Transforms the Select of a Query into an SqlPart
generateSql :: Select -> SqlPart
generateSql (Select (sAgg, aggExpr) sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitAggregation sAgg aggExpr <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere

emitSelect :: SqlPart
emitSelect = emit "SELECT"

emitExpr :: Expr -> SqlPart
emitExpr (Literal (Value v)) = emitParameter v
emitExpr (Column identifier) = emitIdentifier identifier
emitExpr (PrefixOp identifier expr) =
  emit identifier <>
  emitExpr expr
emitExpr (PostfixOp identifier expr) =
  emitExpr expr <>
  emit identifier
emitExpr (BinaryOp expr1 identifier expr2) =
  emitExpr expr1 <>
  emit identifier <>
  emitExpr expr2
emitExpr (FunctionCall identifier exprs) =
  emit identifier <>
  emit "(" <>
  foldMap emitExpr exprs <>
  emit ")"
emitExpr (Case branch branches elseExpr) =
  emit "CASE " <>
  emitBranch branch <>
  emitBranches branches <>
  emitElse <>
  emit "END"
 where
  emitBranches = foldMap emitBranch
  emitBranch (cond, expr) =
    emit "WHEN " <>
    emitExpr cond <>
    emit " THEN " <>
    emitExpr expr <>
    emit " "
  emitElse =
    maybe mempty (\e -> emit "ELSE " <> emitExpr e <> emit " ") elseExpr
emitExpr Null = emit "NULL"
emitExpr Star = emit "*"

emitAggregation :: Aggregation -> Expr -> SqlPart
emitAggregation agg expr =
  emit (aggregationName agg) <>
  emit "(" <>
  emitExpr expr <>
  emit ")"
 where
  aggregationName Average = "AVG"
  aggregationName Sum = "SUM"
  aggregationName Count = "COUNT"

emitWhere :: Maybe Expr -> SqlPart
emitWhere Nothing = mempty
emitWhere (Just expr) =
  emit "WHERE " <>
  emitExpr expr

emitFrom :: Identifier -> SqlPart
emitFrom identifier =
  emit "FROM " <>
  emitIdentifier identifier
