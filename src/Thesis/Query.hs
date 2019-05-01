module Thesis.Query where

import Thesis.Ast
import Thesis.Types
import Thesis.ValueGuard

data Query = Query { getEpsilon :: Positive Epsilon, getDelta :: NonNegative Delta, ast :: Select, defaultAnswer :: Double }
