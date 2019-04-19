module Thesis.Query where

import Thesis.Ast
import Thesis.Types

data Query = Query { getEpsilon :: Epsilon, getDelta :: Delta, ast :: Ast, defaultAnswer :: Double }
  deriving (Show)
