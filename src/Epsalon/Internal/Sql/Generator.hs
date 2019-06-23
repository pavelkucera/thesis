{-# LANGUAGE OverloadedStrings #-}

module Epsalon.Internal.Sql.Generator where

import Data.Text (Text)
import Data.List (intersperse)
import Epsalon.Internal.Query.Ast
import Epsalon.Internal.Sql.Builder

-- | Clips expression E into range [-1, 1] by using LEAST(GREATEST(E, -1), 1).
-- Necessary to limit function sensitivity.
clip :: Expr -> Expr
clip Star = Star
clip Null = Null
clip e =
  let greatest = FunctionCall $ GreatestFn e  (Literal $ Value (negate 1 :: Int))
  in FunctionCall $ LeastFn greatest (Literal $ Value (1 :: Int))

-- | High-level function to emit a query fetching a value for the Laplace mechanism.
-- Offloads computation to the database.
emitLaplace :: DatabaseAggregation -> AggregationAst -> SqlPart
emitLaplace aggregation (AggregationAst expr sFrom sWhere) =
  emitSelect <>
  emit " " <>
  emitAggregation aggregation (clip expr) <>
  emit " " <>
  emitFrom sFrom <>
  emit " " <>
  emitWhere sWhere

-- | High-level function to emit a query fetching values for the exponential mechanism.
-- Results have to be aggregated in-memory.
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
  emit (opName identifier) <>
  emit " " <>
  emitExpr expr
 where
  opName NotOp = "NOT"
emitExpr (PostfixOp identifier expr) =
  emitExpr expr <>
  emit " " <>
  emitOp identifier
 where
  emitOp (IsOp value) = emit "IS " <> truthValue value
  emitOp (IsNotOp value) = emit "IS NOT " <> truthValue value
  truthValue Nothing = emit "NULL"
  truthValue (Just True) = emit "TRUE"
  truthValue (Just False) = emit "FALSE"

emitExpr (BinaryOp identifier left right) =
  emit "(" <>
  emitExpr left <>
  emit ")" <>
  emit (opName identifier) <>
  emit "(" <>
  emitExpr right <>
  emit ")"
 where
  opName MultiplyOp = "*"
  opName DivideOp = "/"
  opName AddOp = "+"
  opName SubtractOp = "-"
  opName EqualOp = "="
  opName GreaterOp = ">"
  opName GreaterOrEqualOp = ">="
  opName LessOp = "<"
  opName LessOrEqualOp = "*"
  opName AndOp = "AND"
  opName OrOp = "OR"
emitExpr (FunctionCall fn) =
  case fn of
    GreatestFn e1 e2 -> emitFunction "GREATEST" [e1, e2]
    LeastFn e1 e2 -> emitFunction "LEAST" [e1, e2]
 where
  emitFunction :: Text -> [Expr] -> SqlPart
  emitFunction identifier exprs =
    emit identifier <>
    emit "(" <>
    mconcat (intersperse (emit ",") (map emitExpr exprs)) <>
    emit ")"
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
