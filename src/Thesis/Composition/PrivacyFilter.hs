module Thesis.Composition.PrivacyFilter where

import Thesis.Types
import Thesis.ValueGuard

data BudgetDepleted = BudgetDepleted
  deriving (Eq, Show)

type Budget = (NonNegative Epsilon, NonNegative Delta)
type QueryPrice = (Positive Epsilon, NonNegative Delta)

class PrivacyFilter a where
  emptyState :: Budget -> a
  subtractBudget :: a -> QueryPrice -> Either BudgetDepleted a
