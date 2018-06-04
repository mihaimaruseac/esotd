{-# LANGUAGE RecordWildCards #-}

import Data.Complex (realPart, imagPart)

import Args (parseArgs, Args(..))
import ExponentialSum (expSums)
import Plot (display)

main :: IO ()
main = do
  Args{..} <- parseArgs
  display $ select start end $ getPoints coeffs
  where
    getPoints = map toCoords . expSums . map fromIntegral
    select st en = take (en - st) . drop st
    toCoords z = (realPart z, imagPart z)
