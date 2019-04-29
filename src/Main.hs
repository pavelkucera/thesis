{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Data.Scientific
import Data.Text (unpack)
import GHC.Generics (Generic)
import Data.Time.Units (Microsecond)

import Thesis.Ast
import Thesis.SqlRunner

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Generic, FromRow, Show)

main :: IO ()
main = do
  let queryEpsilon = 0.1;
  let ast = Select (Average (Column "salary")) ("people") (Just (BinaryOp (Column "id") "<" (Literal (Value (200 :: Integer)))))
  let timeout = 10000 :: Microsecond
  let defaultAnswer = [Only $ fromFloatDigits (1.0 :: Double)]
  conn <- connectPostgreSQL connStr
  output <- executeSql timeout defaultAnswer conn ast queryEpsilon
  print $ unpack $ output
