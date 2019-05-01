module Thesis.Query where

import Thesis.Ast
import Thesis.Types
import Thesis.ValueGuard

data Query = Query { queryEpsilon :: Positive Epsilon, queryAst :: Select }
