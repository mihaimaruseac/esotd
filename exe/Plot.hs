{-|
Module      : Plot
Description : Plot the new image

Plots the result of the exponential sum of the day.
 -}

module Plot (display) where

import Diagrams.Backend.Rasterific
import Diagrams

display :: [(Double, Double)] -> Diagram B
display = pad 1.1 . centerXY . fromVertices . map p2
