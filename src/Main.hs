{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Data.Scientific

connStr :: ByteString
connStr = "host=localhost dbname=leendert user=leendert password=leendert"

data Person = Person { id :: Int, firstName :: String, lastName :: String, email :: String, gender :: String, salary :: Scientific, birthdate :: Date }
  deriving (Generic, FromRow, Show)

main :: IO ()
main = do
  let queryString = "SELECT id, first_name, last_name, email, gender, salary, birthdate FROM people"
  conn <- connectPostgreSQL connStr
  result <- (query_ conn queryString)
  print (result :: [Person])
