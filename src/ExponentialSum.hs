-- | Example of a library file. It is also used for testing the test suites.
module ExponentialSum
  (
    -- * Exported functions
    term
  ) where

-- | Compute the value of the nth term, given the initial coefficients
term :: [Double] -> Double -> Double
term xs n = sum $ zipWith sumTerm xs [1..]
  where
    sumTerm :: Double -> Int -> Double
    sumTerm x i = n ^ i / x
