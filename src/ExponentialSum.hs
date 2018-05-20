{-|
Module      : ExponentialSum
Description : Compute the exponential sum of the day

Compute the exponential sum of the day, to be plotted later.
 -}

module ExponentialSum where

import Data.Complex

-- | Compute the value of the real argument of the exponential, at point n,
-- given the initial coefficients.
--
-- For example, here is the third value computed where the coefficients are
-- current date:
--
-- >>> realArg [12, 5, 2018] 3
-- 2.063379583746283
-- >>> realArg [12, 5, 18] 3
-- 3.55
--
-- There are some properties which might be useful:
--
-- 1. If the coefficients are missing, we want to return a 0 for any index:
--
-- prop> realArg [] x == 0
--
-- 2. If the index is 0, we output 0 regardless of the coefficients, provided
-- at least one is non-zero
--
-- prop> a == 0 || realArg (a:xs) 0 == 0
--
-- There might be a few more properties but we can ignore them for now, as
-- some might fail due to numerical inaccuracies. We are also sidestepping
-- NaNs and infinities at the moment.
realArg :: [Double] -> Double -> Double
realArg xs n = sum $ zipWith power xs [1..]
  where
    power :: Double -> Int -> Double
    power x i = n ^ i / x

-- | Computes the value of the complex exponential for a term at an index
expVal :: [Double] -> Double -> Complex Double
expVal xs n = exp $ 2 * pi * i * (realArg xs n :+ 0)
  where
    i = 0 :+ 1
