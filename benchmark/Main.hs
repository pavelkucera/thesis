{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types

import Data.ByteString (ByteString)
import Data.Scientific
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import GHC.Generics (Generic)
import System.Random

import Thesis.Ast
import Thesis.Composition.Simple
import Thesis.Query
import Thesis.QueryRunner (run)
import Thesis.ValueGuard (positive, nonNegative)

import Thesis.Ast

query_with :: Select
query_with = Select (Sum, FunctionCall "factorial" [Literal . Value $ (10000 :: Int)]) "people" (Just $ BinaryOp (Column "first_name") "=" (Literal $ Value ("Jon" :: String)))

query_without :: Select
query_without = Select (Sum, FunctionCall "factorial" [Literal . Value $ (10000 :: Int)]) "people" (Just $ BinaryOp (Column "first_name") "=" (Literal $ Value ("Daenerys" :: String)))

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

mconfig = defaultConfig {
  resamples = 1000
}

main :: IO ()
main = do
   dbh <- connectPostgreSQL connStr
   r <- defaultMainWith mconfig [
    bgroup "protected" [
        bench "with-record" $ nfAppIO (runTest dbh) query_with,
        bench "without-record" $ nfAppIO (runTest dbh) query_without
      ]
    ]

   return r

runTest :: Connection -> Select -> IO Double
runTest connection ast =
  let (qEpsilon, budgetEpsilon, budgetDelta) =
        case (positive 0.1, nonNegative 1, nonNegative 0) of
          (Right e1, Right e2, Right d) -> (e1, e2, d)
      myQuery = Query qEpsilon ast
      privacyFilter = SimpleCompositionState budgetEpsilon budgetDelta
  in do
    gen <- getStdGen
    (_, newGen, output) <- run gen connection privacyFilter myQuery
    setStdGen newGen
    return $ case output of
      Left err -> 0
      Right result -> result
