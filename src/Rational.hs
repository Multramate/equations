module Rational where

import Data.Ratio ((%), numerator, denominator)

import Equation

--------------------------------------------------------------------------------

instance Num Numeric where
  Z z + Z z' = Z (z + z')
  Z z + q @ (Q _ _) = toQ (fromIntegral z + fromQ q)
  Z z + R r = R (fromIntegral z + r)
  q @ (Q _ _) + Z z = toQ (fromQ q + fromIntegral z)
  q @ (Q _ _) + q' @ (Q _ _) = toQ (fromQ q + fromQ q')
  q @ (Q _ _) + R r = R (fromRational (fromQ q) + r)
  R r + Z z = R (r + fromIntegral z)
  R r + q @ (Q _ _) = R (r + fromRational (fromQ q))
  R r + R r' = R (r + r')
  Z z * Z z' = Z (z * z')
  Z z * q @ (Q _ _) = toQ (fromIntegral z * fromQ q)
  Z z * R r = R (fromIntegral z * r)
  q @ (Q _ _) * Z z = toQ (fromQ q * fromIntegral z)
  q @ (Q _ _) * q' @ (Q _ _) = toQ (fromQ q * fromQ q')
  q @ (Q _ _) * R r = R (fromRational (fromQ q) * r)
  R r * Z z = R (r * fromIntegral z)
  R r * q @ (Q _ _) = R (r * fromRational (fromQ q))
  R r * R r' = R (r * r')
  abs (Z z) = Z (abs z)
  abs q @ (Q _ _) = toQ (abs (fromQ q))
  abs (R r) = R (abs r)
  signum (Z z) = Z (signum z)
  signum q @ (Q _ _) = toQ (signum (fromQ q))
  signum (R r) = R (signum r)
  fromInteger = Z . fromIntegral
  negate (Z z) = Z (negate z)
  negate q @ (Q _ _) = toQ (negate (fromQ q))
  negate (R r) = R (negate r)

instance Fractional Numeric where
  fromRational = toQ . fromRational
  recip (Z z) = toQ (recip (fromIntegral z))
  recip q @ (Q _ _) = toQ (recip (fromQ q))
  recip (R r) = R (recip r)

--------------------------------------------------------------------------------

fromQ :: Numeric -> Rational
fromQ (Z z) = fromIntegral z % 1
fromQ (Q n d) = fromIntegral n % fromIntegral d
fromQ (R r) = approxQ (r, 1)

approxQ :: (Double, Integer) -> Rational
approxQ (n, d)
  | n == fromIntegral (floor n) = floor n % d
  | otherwise = approxQ (10 * n, 10 * d)

toQ :: Rational -> Numeric
toQ = Q . fromIntegral . numerator <*> fromIntegral . denominator