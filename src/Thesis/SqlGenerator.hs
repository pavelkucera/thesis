{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlGenerator where

import Thesis.Ast
import Thesis.SqlBuilder

emitLaplace :: DatabaseSelect -> SqlPart
emitLaplace (DatabaseSelect (agg, aggExpr) sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitAggregation agg aggExpr <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere

emitExponential :: StreamSelect -> SqlPart
emitExponential (StreamSelect (_, expr) sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitExpr expr <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere <>
  emit " " <>
  emitOrderBy expr

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

emitAggregation :: DatabaseAggregation -> Expr -> SqlPart
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

emitOrderBy :: Expr -> SqlPart
emitOrderBy expr =
  emit "ORDER BY" <>
  emitExpr expr

emitCount :: StreamSelect -> SqlPart
emitCount (StreamSelect (_, aggExpr) sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitAggregation Count aggExpr <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere
