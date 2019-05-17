{-# LANGUAGE GADTs #-}

module Thesis.Ast where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.ToField (ToField)

type Identifier = Text

data Value where
  Value :: (Eq a, ToField a, Show a, Typeable a) => a -> Value

instance Show Value where
  show (Value x) = show x

data Expr =
    Literal Value
  | Column Identifier
  | PrefixOp Identifier Expr
  | PostfixOp Identifier Expr
  | BinaryOp Expr Identifier Expr
  | FunctionCall Identifier [Expr]
  | Case (Expr, Expr) [(Expr, Expr)] (Maybe Expr)
  | Null
  | Star
  deriving Show

data DatabaseAggregation = Sum | Average | Count
  deriving Show
data StreamAggregation = Median | Min | Max
  deriving Show

data DatabaseSelect = DatabaseSelect {
  selectAggregationD :: (DatabaseAggregation, Expr),
  selectFromD :: Identifier,
  selectWhereD :: Maybe Expr
} deriving Show

data StreamSelect = StreamSelect {
  selectAggregationS :: (StreamAggregation, Expr),
  selectFromS :: Identifier,
  selectWhereS :: Maybe Expr
} deriving Show
