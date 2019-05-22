{-# LANGUAGE DeriveLift #-}

module Thesis.Query.Query where

import Language.Haskell.TH.Syntax

import Thesis.Query.Ast
import Thesis.Types
import Thesis.ValueGuard

data Query = Query {
  queryEpsilon :: Positive Epsilon,
  queryAst :: Aggregation
} deriving (Lift)
