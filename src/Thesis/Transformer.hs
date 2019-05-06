{-# LANGUAGE OverloadedStrings #-}

module Thesis.Transformer where

import Thesis.Ast

negateQuery :: Select -> Select
negateQuery select =
  let newWhere = fmap negateExpr $ selectWhere select
  in select { selectWhere = newWhere }

negateExpr :: Expr -> Expr
negateExpr e = FunctionCall "NOT" [e]
