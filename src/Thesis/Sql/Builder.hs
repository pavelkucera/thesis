{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Sql.Builder where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable, cast)
import Database.PostgreSQL.Simple.Types (Identifier(..), Query(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

-- | A parameter which is to be escaped before sending to the database.
data Parameter where
  Parameter :: (Eq a, Show a, ToField a, Typeable a) => a -> Parameter

instance Eq Parameter where
  (Parameter x) == (Parameter y) =
    cast x == Just y

instance Show Parameter where
  show (Parameter x) = show x

instance ToField Parameter where
  toField (Parameter x) = toField x

-- | Representation of an SQL query. Do not use directly, to prevent introducing
-- SQL injection. Use 'emit', 'emitIdentifier' and 'emitParameter' to build the SQL.
--
-- The type is an instance of 'Monoid', and the emit functions can be thus linked
-- using '<>'.
data SqlPart =
  SqlPart Text [Parameter]
  deriving (Eq, Show)

instance Semigroup SqlPart where
  (SqlPart lSql lPs) <> (SqlPart rSql rPs) =
    SqlPart (lSql <> rSql) (lPs <> rPs)

instance Monoid SqlPart where
  mempty = SqlPart mempty mempty

-- | Use to emit raw, unescaped SQL without any parameters. The SQL should not
-- contain any question marks, which are used as parameter placeholders.
emit :: Text -> SqlPart
emit sql = SqlPart sql mempty

-- | Use to safely emit unknown text as a part of the SQL. Use for escaping
-- user input etc. Escaping is offloaded to the underlying database client.
emitParameter :: (Eq a, Show a, ToField a, Typeable a) => a -> SqlPart
emitParameter p = SqlPart "?" [Parameter p]

-- | Use to safely emit an identifier.
emitIdentifier :: Text -> SqlPart
emitIdentifier i = emitParameter $ Identifier i

-- | Transforms SqlPart into a query representation used by the underlying
-- database client.
toQuery :: SqlPart -> (Query, [Parameter])
toQuery (SqlPart sql ps) = (Query $ encodeUtf8 sql, ps)
