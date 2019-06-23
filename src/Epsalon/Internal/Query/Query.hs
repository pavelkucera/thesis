module Epsalon.Internal.Query.Query where

import Epsalon.Internal.Query.Ast
import Epsalon.Internal.Types
import Epsalon.Internal.ValueGuard

data Query = Query {
  queryEpsilon :: Positive Epsilon,
  queryAst :: Aggregation
}
