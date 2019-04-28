{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Data.Scientific
import System.Random

import Thesis.SqlBuilder
import Thesis.LaplaceNoise
import Thesis.Ast

connStr :: ByteString
connStr = "host=localhost dbname=postgres user=postgres password=password"

data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Generic, FromRow, Show)

main :: IO ()
main = do
  let queryEpsilon = 0.1;
  let ast = Select (Average (Column "salary")) ("people") (Just (BinaryOp (Column "id") "<" (Literal (Value (200 :: Integer)))))
  print $ selectToQuery ast
  let sqlPart = emit "SELECT AVG(salary) FROM people"
  let sqlQuery = toQuery sqlPart
  gen <- getStdGen
  conn <- connectPostgreSQL connStr
  result <- (query conn (fst sqlQuery) (snd sqlQuery))
  let sensitivity = 1
  let scale = queryEpsilon / sensitivity
  let (noise, newGen) = generate gen scale
  print $ (fromOnly $ head $ (result :: [Only Scientific])) + fromFloatDigits noise
  let q2 = selectToQuery ast
  result2 <- (query conn (fst q2) (snd q2))
  let (noise2, newGen2) = generate newGen scale
  setStdGen $ newGen2
  print $ (fromOnly $ head $ (result2 :: [Only Scientific])) + fromFloatDigits noise2
