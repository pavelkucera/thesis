{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.ByteString (ByteString)
import Data.Scientific
import Data.Text (unpack)
import Data.Time.Units (Microsecond)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import GHC.Generics (Generic)
import System.Random

import Thesis.Ast
import Thesis.Query
import Thesis.QueryRunner (runQuery)
import Thesis.ValueGuard (positive, nonNegative)

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Generic, FromRow, Show)

main :: IO ()
main =
  let myAst = Select (Average (Column "salary")) ("people") (Just (BinaryOp (Column "id") "<" (Literal (Value (200 :: Integer)))))
      (epsilon, delta) = case (positive 0.1, nonNegative 0) of
        (Right e, Right d) -> (e, d)
        (Left err , _) -> error $ show err
        (_, Left err ) -> error $ show err
      myQuery = Query epsilon delta myAst 0.5
      timeout = 10000 :: Microsecond
  in do
     conn <- connectPostgreSQL connStr
     gen <- getStdGen
     (output, newGen) <- runQuery timeout conn gen myQuery
     setStdGen newGen
     print $ unpack $ output
