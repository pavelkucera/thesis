{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlGenerator.Internal where

import Thesis.Ast
import Thesis.Query
import Thesis.SqlBuilder

-- | Transforms the Select of a Query into an SqlPart
generateSql :: Query -> SqlPart
generateSql (Query _ (Select sAgg sFrom sWhere)) =
  emitSelect <>
  emit " " <>
  emitAggregation sAgg <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere

emitSelect :: SqlPart
emitSelect = emit "SELECT"

emitExpr :: Expr -> SqlPart
emitExpr (Literal (Value v)) = emitParameter $ v
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

emitAggregation :: Aggregation -> SqlPart
emitAggregation (Average expr) =
  emit "AVG(" <>
  emitExpr expr <>
  emit ")"
emitAggregation (Sum expr) =
  emit "SUM(" <>
  emitExpr expr <>
  emit ")"
emitAggregation (Count countExpr) =
  emit "COUNT(" <>
  (case countExpr of
      Star           -> emit "*"
      CountExpr expr -> emitExpr expr) <>
  emit ")"

emitWhere :: Maybe Expr -> SqlPart
emitWhere Nothing = mempty
emitWhere (Just expr) =
  emit "WHERE " <>
  emitExpr expr

emitFrom :: Identifier -> SqlPart
emitFrom identifier =
  emit "FROM " <>
  emitIdentifier identifier
