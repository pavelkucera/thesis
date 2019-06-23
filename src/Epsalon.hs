module Epsalon (
  runString,
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
import Epsalon.Internal.Query.Query
import Epsalon.Internal.Query.Runner
import Epsalon.Internal.Types as X (Epsilon, Delta)
import Epsalon.Internal.ValueGuard as X (Positive, NonNegative, positive, nonNegative, value, zero)

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
