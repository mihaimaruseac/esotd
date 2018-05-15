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
term :: [Double] -> Double -> Double
term xs n = sum $ zipWith sumTerm xs [1..]
  where
    sumTerm :: Double -> Int -> Double
    sumTerm x i = n ^ i / x
