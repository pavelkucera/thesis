{-# LANGUAGE OverloadedStrings #-}

module Thesis.Transformer where

import Thesis.Ast
import Thesis.Query

negateQuery :: Query -> Query
negateQuery query =
  let newAst = negateSelect $ queryAst query
  in query { queryAst = newAst }

negateSelect :: Select -> Select
negateSelect select =
  let newWhere = negateExpr <$> selectWhere select
  in select { selectWhere = newWhere }

negateExpr :: Expr -> Expr
negateExpr e = PostfixOp "IS NOT TRUE" e
