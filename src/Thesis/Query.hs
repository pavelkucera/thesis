module Thesis.Query where

import Thesis.Ast
import Thesis.Types
import Thesis.ValueGuard

data Query a = Query {
  queryEpsilon :: Positive Epsilon,
  queryAst :: SelectAst a
}
