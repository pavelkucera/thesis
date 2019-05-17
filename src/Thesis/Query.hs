module Thesis.Query where

import Thesis.Ast
import Thesis.Types
import Thesis.ValueGuard

data DatabaseQuery = DatabaseQuery (Positive Epsilon) DatabaseSelect
data StreamQuery = StreamQuery (Positive Epsilon) StreamSelect

data Query = DQuery DatabaseQuery | SQuery StreamQuery

queryEpsilon :: Query -> Positive Epsilon
queryEpsilon query = case query of
  DQuery (DatabaseQuery e _) -> e
  SQuery (StreamQuery e _)   -> e
