{-|
Module      : Plot
Description : Plot the new image

Plots the result of the exponential sum of the day.
 -}

module Plot (display) where

import Diagrams.Backend.Rasterific
import Diagrams

display :: [(Double, Double)] -> Diagram B
display _ = circle 1
