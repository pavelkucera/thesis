{-# LANGUAGE TypeFamilies #-}

module Epsalon.Internal.Composition.PrivacyFilter where

import Epsalon.Internal.Types
import Epsalon.Internal.ValueGuard

data BudgetDepleted = BudgetDepleted
  deriving (Eq, Show)

type QueryPrice = (Positive Epsilon, NonNegative Delta)

class PrivacyFilter a where
  type Budget a :: *
  emptyState :: Budget a -> a
  subtractBudget :: a -> QueryPrice -> Either BudgetDepleted a
