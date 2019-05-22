{-# LANGUAGE OverloadedStrings #-}

module Thesis.Sql.Generator where

import Data.Text (Text)
import Data.List (intersperse)
import Thesis.Query.Ast
import Thesis.Sql.Builder

clip :: Expr -> Expr
clip Star = Star
clip Null = Null
clip e =
  let greatest = FunctionCall "GREATEST" [e, Literal $ Value (negate 1 :: Int)]
  in FunctionCall "LEAST" [greatest, Literal $ Value (1 :: Int)]

emitLaplace :: DatabaseAggregation -> AggregationAst -> SqlPart
emitLaplace aggregation (AggregationAst expr sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitAggregation aggregation (clip expr) <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere

emitExponential :: StreamAggregation -> AggregationAst -> SqlPart
emitExponential _ (AggregationAst expr sFrom sWhere) =
  let valueExpr = clip expr in
  emitSelect <>
  emit " " <>
  emitExpr valueExpr <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere <>
  emit " " <>
  emitOrderBy valueExpr

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
emitExpr (BinaryOp identifier left right) =
  emit "(" <>
  emitExpr left <>
  emit ")" <>
  emit identifier <>
  emit "(" <>
  emitExpr right <>
  emit ")"
emitExpr (FunctionCall identifier exprs) =
  emit identifier <>
  emit "(" <>
  mconcat (intersperse (emit ",") (map emitExpr exprs)) <>
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
emitAggregation aggregation expr =
  emit (aggregationName aggregation) <>
  emit "(" <>
  emitExpr expr <>
  emit ")"
 where
  aggregationName :: DatabaseAggregation -> Text
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
  emit "ORDER BY " <>
  emitExpr expr

emitCount :: AggregationAst -> SqlPart
emitCount (AggregationAst expr sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitAggregation Count expr <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere
