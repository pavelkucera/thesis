module Epsalon (
  executeQuery,
  executeStringQuery,
  module X
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)
import Text.Megaparsec.Error (ParseErrorBundle)

import Epsalon.Internal.Parser (parseQuery)
import Epsalon.Internal.Composition.Adaptive as X (AdaptiveCompositionState(..))
import Epsalon.Internal.Composition.PrivacyFilter as X (BudgetDepleted, PrivacyFilter(..), QueryPrice)
import Epsalon.Internal.Composition.Simple as X (SimpleCompositionState(..))
import Epsalon.Internal.Query.Ast as X
import Epsalon.Internal.Query.Query as X
import Epsalon.Internal.Query.Runner
import Epsalon.Internal.Types as X (Epsilon, Delta)
import Epsalon.Internal.ValueGuard as X (Positive, NonNegative, positive, nonNegative, value, zero)

executeQuery :: (MonadIO m, PrivacyFilter p)
             => StdGen
             -> Connection
             -> p
             -> Query
             -> m (p, StdGen, Either BudgetDepleted Double)
executeQuery gen conn privacyFilter query = do
  (newState, newGen, queryResult) <- run gen conn privacyFilter query
  return $ case queryResult of
    Right res -> (newState, newGen, Right res)
    Left err -> (newState, newGen, Left err)

executeStringQuery :: (MonadIO m, PrivacyFilter p)
                   => StdGen
                   -> Connection
                   -> p
                   -> Positive Epsilon
                   -> String
                   -> m (p, StdGen, Either (Either BudgetDepleted (ParseErrorBundle String Void)) Double)
executeStringQuery gen conn privacyFilter e queryString =
  case parseQuery queryString of
    Right aggregation -> do
      (newState, newGen, result) <- executeQuery gen conn privacyFilter (Query e aggregation)
      case result of
        Right result' -> return (newState, newGen, Right result')
        Left err -> return (newState, newGen, Left (Left err))
    Left err -> return (privacyFilter, gen, Left (Right err))
