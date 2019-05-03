{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Thesis.ValueGuard (
  Positive,
  NonPositive,
  Negative,
  NonNegative,
  positive,
  nonNegative,
  value,
  zero
) where

-- | Wrapper for positive values. Use 'positive' to construct a value of this type.
newtype Positive a = Positive a
  deriving (Eq, Ord, Show)

-- | Wrapper for non-positive values. Use 'positive' to construct a value of this type.
newtype NonPositive a = NonPositive a
  deriving (Eq, Show)

-- | Wrapper for non-negative values. Use 'nonNegative' to construct a value of this type.
newtype NonNegative a = NonNegative a
  deriving (Eq, Ord, Show)

-- | Wrapper for negative values. Use 'nonNegative' to construct a value of this type.
newtype Negative a = Negative a
  deriving (Eq, Show)

-- | Takes a numeric value and if the value is strictly greater than zero, the function
-- returns 'Positive'. Otherwise it returns 'NonPositive'.
--
-- >>> positive 42
-- Right (Positive 42)
--
-- >>> positive (-42)
-- Left (NonPositive (-42))
positive :: (Num a, Ord a) => a -> Either (NonPositive a) (Positive a)
positive x | x > 0 = Right $ Positive x
           | otherwise = Left $ NonPositive x

-- | Takes a numeric value and if the value is greather than or equal to zero, the
-- function returns 'NonNegative'. Otherwise it returns 'Negative'.
--
-- >>> nonNegative 0
-- Right (NonNegative 0)
--
-- >>> nonNegative (-42)
-- Left (Negative (-42))
nonNegative :: (Num a, Ord a) => a -> Either (Negative a) (NonNegative a)
nonNegative x | x >= 0 = Right $ NonNegative x
              | otherwise = Left $ Negative x

-- | Typeclass to abstract extraction of values from wrapped types.
class WrappedValue a b | a -> b where
  value :: a -> b

instance WrappedValue (Positive a) a where
  value (Positive x) = x

instance WrappedValue (NonNegative a) a where
  value (NonNegative x) = x

zero :: (Num a) => NonNegative a
zero = NonNegative 0
