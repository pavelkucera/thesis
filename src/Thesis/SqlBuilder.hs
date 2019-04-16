{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlBuilder where

import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

data Parameter where
  Parameter :: (Eq a, Show a, ToField a, Typeable a) => a -> Parameter

instance Eq Parameter where
  (Parameter x) == (Parameter y) =
    cast x == Just y

instance Show Parameter where
  show (Parameter x) = show x

instance ToField Parameter where
  toField (Parameter x) = toField x

data SqlPart =
  SqlPart Text [Parameter]
  deriving (Eq, Show)

instance Semigroup SqlPart where
  (SqlPart lSql lPs) <> (SqlPart rSql rPs) =
    SqlPart (lSql <> rSql) (lPs <> rPs)

instance Monoid SqlPart where
  mempty = SqlPart mempty mempty

emit :: Text -> SqlPart
emit s = SqlPart s mempty

emitParameter :: (Eq a, Show a, ToField a, Typeable a) => a -> SqlPart
emitParameter p = SqlPart "?" [Parameter p]

emitIdentifier :: Text -> SqlPart
emitIdentifier i = emitParameter $ Identifier i
