{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString (ByteString)
import Data.Scientific
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import System.Random

import Thesis.Query.Ast
import Thesis.Composition.Simple
import Thesis.Query.Query
import Thesis.Query.Runner (run)
import Thesis.Parser (queryQQ)
import Thesis.ValueGuard (positive, nonNegative)

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

-- | Database schema
data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Show)

main :: IO ()
main =
  let parsedAst = [queryQQ|SELECT MEDIAN(salary) FROM people WHERE id > 0|]
      myAst = StreamAggregation Median $ AggregationAst (Column "salary") "people" (Just (BinaryOp ">" (Column "id") (Literal (Value (0 :: Integer)))))
      (qEpsilon, budgetEpsilon, budgetDelta) = case (positive 0.1, nonNegative 1, nonNegative 0) of
        (Right e1, Right e2, Right d) -> (e1, e2, d)
        (Left err, _, _) -> error $ show err
        (_, Left err, _) -> error $ show err
        (_, _, Left err) -> error $ show err
      myQuery = Query qEpsilon myAst
      privacyFilter = SimpleCompositionState budgetEpsilon budgetDelta
  in do
     print parsedAst
     conn <- connectPostgreSQL connStr
     gen <- getStdGen
     (_, newGen, output) <- run gen conn privacyFilter myQuery
     setStdGen newGen
     case output of
       Left err -> print err
       Right result -> print result
