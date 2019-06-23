module Epsalon.Query.Query where

import Epsalon.Query.Ast
import Epsalon.Types
import Epsalon.ValueGuard

data Query = Query {
  queryEpsilon :: Positive Epsilon,
  queryAst :: Aggregation
}
