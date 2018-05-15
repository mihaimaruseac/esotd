-- | Example of a library file. It is also used for testing the test suites.
module ExponentialSum
  (
    -- * Exported functions
    term
  ) where

-- | Compute the value of the nth term, given the initial coefficients
--
-- For example, here is the third term computed where the coefficients are
-- current date:
--
-- >>> term [12, 5, 2018] 3
-- 2.063379583746283
-- >>> term [12, 5, 18] 3
-- 3.55
--
-- There are some properties which might be useful:
--
-- 1. If the coefficients are missing, we want to return a 0 for any term
-- index:
--
-- prop> term [] x == 0
--
-- 2. If the term is 0, we output 0 regardless of the coefficients, provided
-- at least one is non-zero
--
-- prop> a == 0 || term (a:xs) 0 == 0
--
-- There might be a few more properties but we can ignore them for now, as
-- some might fail due to numerical inaccuracies. We are also sidestepping
-- NaNs and infinities at the moment.
term :: [Double] -> Double -> Double
term xs n = sum $ zipWith sumTerm xs [1..]
  where
    sumTerm :: Double -> Int -> Double
    sumTerm x i = n ^ i / x
