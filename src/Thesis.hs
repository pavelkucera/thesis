module Thesis (
  runString,
  module X
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import System.Random (StdGen)
import Text.Megaparsec.Error (ParseErrorBundle)

import Thesis.Parser (parseQuery)
import Thesis.Composition.Adaptive as X (AdaptiveCompositionState(..))
import Thesis.Composition.PrivacyFilter as X (BudgetDepleted, PrivacyFilter(..), QueryPrice)
import Thesis.Composition.Simple as X (SimpleCompositionState(..))
import Thesis.Query.Query
import Thesis.Query.Runner
import Thesis.Types as X (Epsilon, Delta)
import Thesis.ValueGuard as X (Positive, NonNegative, positive, nonNegative, value, zero)

runString :: (MonadIO m, PrivacyFilter p)
          => StdGen
          -> Connection
          -> p
          -> Positive Epsilon
          -> String
          -> m (p, StdGen, Either (Either BudgetDepleted (ParseErrorBundle String Void)) Double)
runString gen conn privacyFilter e queryString =
  case parseQuery queryString of
    Right aggregation -> do
      (newState, newGen, queryResult) <- run gen conn privacyFilter (Query e aggregation)
      case queryResult of
        Right res -> return (newState, newGen, Right res)
        Left err -> return (newState, newGen, Left (Left err))
    Left err -> return (privacyFilter, gen, Left (Right err))
