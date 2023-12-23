module Main (main) where

import BaseFunctions
import MatrixGen
-- import Math.GaussianQuadratureIntegration (nIntegrate256)


main = print [ [ (i, j) | i <- [1..4] ] | j <- [1..4] ]
