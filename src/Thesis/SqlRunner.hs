{-# LANGUAGE OverloadedStrings #-}

module Thesis.SqlRunner where

import System.Random
import Data.Text (Text, pack)
import Data.Scientific
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Thesis.Ast
import Thesis.LaplaceNoise (generate)
import Thesis.Microtransaction
import Thesis.Types
import Data.Time.Units (Microsecond, TimeUnit)
import Control.DeepSeq (NFData)

executeSql :: (TimeUnit t, NFData a) => t -> a -> Connection -> Select -> Epsilon -> IO Text
executeSql timeout defaultAnswer conn ast e = do
  let (pquery, parameters) = selectToQuery ast
  gen <- getStdGen
  result <- (runMicrotransaction timeout defaultAnswer ((query conn pquery parameters)))
  let sensitivity = getSensitivity (selectAggregation ast)
  let scale = e / sensitivity
  let (noise, newGen) = generate gen scale
  setStdGen newGen
  --return result
  return $ pack $ show $ (fromOnly $ head $ (result)) + fromFloatDigits noise
