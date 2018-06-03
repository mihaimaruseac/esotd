{-# LANGUAGE RecordWildCards #-}

import Args (parseArgs, Args(..))
import ExponentialSum (expSums)

main :: IO ()
main = do
  Args{..} <- parseArgs
  print $ take (end - start) $ drop start $ expSums $ map fromIntegral coeffs
