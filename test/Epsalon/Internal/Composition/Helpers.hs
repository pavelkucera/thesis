module Epsalon.Internal.Composition.Helpers where

import Epsalon.Internal.Types
import Epsalon.Internal.Composition.PrivacyFilter
import Epsalon.Internal.ValueGuard

mkPrice :: Epsilon -> Delta -> QueryPrice
mkPrice epsilon delta =
  case (positive epsilon, nonNegative delta) of
    (Right epsilon, Right delta) -> (epsilon, delta)

countQueries :: PrivacyFilter a => a -> QueryPrice -> Int
countQueries initialState price = loop 0 (Right initialState)
 where
  loop i (Left _) = i - 1
  loop i (Right state) = loop (i + 1) $ subtractBudget state price
