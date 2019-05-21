module Thesis.Query.Query where

import Thesis.Query.Ast
import Thesis.Types
import Thesis.ValueGuard

data Query a = Query {
  queryEpsilon :: Positive Epsilon,
  queryAst :: SelectAst a
}
