{-|
Module      : ExponentialSum
Description : Compute the exponential sum of the day

Compute the exponential sum of the day, to be plotted later.
 -}

module ExponentialSum
  (
    -- * Main function
    expSums
    -- * Exported for convenience
  , expVal
  , realArg
  ) where

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
-- >>> realArg [-1, -2, -3] 4
-- -0.421875
--
-- There are some properties which might be useful:
--
-- 1. If the coefficients are missing, we want to return a 0 for any index:
--
--     prop> realArg [] x == 0
--
-- 2. If the index is 0, we output 0 regardless of the coefficients, provided
-- at least one is non-zero
--
--     prop> a == 0 || realArg (a:xs) 0 == 0
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
-- Since we need to multiply the result of `realArg` with i, we just put it in
-- the imaginary part of the newly minted complex number.
--
-- Examples:
--
-- >>> expVal [12, 5, 2018] 3
-- 0.9217505006686714 :+ 0.3877834634393964
-- >>> expVal [12, 5, 18] 3
-- (-0.9510565162951544) :+ (-0.3090169943749449)
-- >>> expVal [-1, -2, -3] 4
-- 0.9210609940028851 :+ (-0.3894183423086505)
--
-- Also, we have the following properties:
--
-- prop> x == 0 || expVal [] x == 1
-- prop> a >= 0 || expVal (a:xs) 0 == 1
expVal :: [Double] -> Double -> Complex Double
expVal xs n = exp $ 2 * pi * (0 :+ realArg xs n)

-- | Computes the partial sums of the exponential sum of the day
--
-- >>> take 3 $ expSums [12, 5, 18]
-- [1.0 :+ 0.0,0.4700807357667952 :+ 0.8480480961564261,(-0.3779673603896302) :+ 1.3779673603896319]
-- >>> take 3 $ expSums [12, 5, 2018]
-- [1.0 :+ 0.0,0.7890437903507289 :+ 0.9774955128339019,1.7720662136902803 :+ 0.794010049617778]
-- >>> take 3 $ expSums []
-- [1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]
-- >>> take 3 $ expSums [-1, -2, -3]
-- [1.0 :+ 0.0, 1.9210609940028851 :+ (-0.3894183423086505), 2.8421219880057702 :+ (-0.778836684617301)]
expSums :: [Double] -> [Complex Double]
expSums xs = scanl1 (+) $ map (expVal xs) [0..]
