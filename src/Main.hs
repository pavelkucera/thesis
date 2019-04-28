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

connStr :: ByteString
connStr = "host=localhost dbname=leendert user=leendert password=leendert"

data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Generic, FromRow, Show)

main :: IO ()
main = do
  let queryEpsilon = 0.1;
  --let ast = Select (Average (Column "salary")) ("people") (Just (BinaryOp (Column "id") "<" (Literal (Value (200 :: Integer)))))
  let sqlPart = emit "SELECT AVG(salary) FROM people"
  let sqlQuery = toQuery sqlPart
  gen <- getStdGen
  conn <- connectPostgreSQL connStr
  result <- (query conn (fst sqlQuery) (snd sqlQuery))
  let (noise, newGen) = generate gen queryEpsilon
  setStdGen $ newGen
  print $ (fromOnly $ head $ (result :: [Only Scientific])) + fromFloatDigits noise
