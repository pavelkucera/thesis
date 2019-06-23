{-# LANGUAGE TypeFamilies #-}

module Epsalon.Composition.PrivacyFilter where

import Epsalon.Types
import Epsalon.ValueGuard

data BudgetDepleted = BudgetDepleted
  deriving (Eq, Show)

type QueryPrice = (Positive Epsilon, NonNegative Delta)

class PrivacyFilter a where
  type Budget a :: *
  emptyState :: Budget a -> a
  subtractBudget :: a -> QueryPrice -> Either BudgetDepleted a
