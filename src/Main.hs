{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.ByteString (ByteString)
import Data.Scientific
import Data.Time.Units (Microsecond)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import GHC.Generics (Generic)
import System.Random

import Thesis.Ast
import Thesis.Composition.Simple
import Thesis.Query
import Thesis.QueryRunner (run)
import Thesis.ValueGuard (positive, nonNegative)

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Generic, FromRow, Show)

main :: IO ()
main =
  let myAst = Select (Average (Column "salary")) ("people") (Just (BinaryOp (Column "id") "<" (Literal (Value (200 :: Integer)))))
      (qEpsilon, budgetEpsilon, budgetDelta) = case (positive 0.1, nonNegative 1, nonNegative 0) of
        (Right e1, Right e2, Right d) -> (e1, e2, d)
        (Left err, _, _) -> error $ show err
        (_, Left err, _) -> error $ show err
        (_, _, Left err) -> error $ show err
      myQuery = Query qEpsilon myAst
      timeout = 100000 :: Microsecond
      privacyFilter = SimpleCompositionState budgetEpsilon budgetDelta
  in do
     conn <- connectPostgreSQL connStr
     gen <- getStdGen
     (_, newGen, output) <- run gen conn privacyFilter myQuery timeout
     setStdGen newGen
     case output of
       Left err -> print err
       Right result -> print $ result
