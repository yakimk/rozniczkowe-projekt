module MatrixGen(generateBs, generateLs, coefficients, approxU)where

import BaseFunctions
import Spec
import Math.GaussianQuadratureIntegration(nIntegrate256)
import Data.Maybe (fromJust)
import Numeric.LinearAlgebra (flatten, fromLists, linearSolve, luSolve, toList, toLists, (><))

approxU :: Double -> Double
approxU x = sum . zipWith (*) coefficients $ basisF x
  where
    basisF x = [fromJust (eI n i) x | i <- [0..n]]

coefficients :: [Double]
coefficients = toList . flatten $ fromJust (linearSolve b_matrix l_vector)
  where
    b_matrix = fromLists generateBs 
    l_vector = (n >< 1) generateLs

n :: Int
n = defaultN

generateLs :: [Double]
generateLs = [generateL i | i <- [0..n]]

generateL :: Int -> Double
generateL i 
    |i  == 0 = 20
    |otherwise = 0

generateBs :: [[Double]]
generateBs = [ [ generateB i j | i <- [1..n] ] | j <- [1..n] ]

generateB :: Int -> Int -> Double
generateB i j  
    |abs(i - j) > 1 = 0
    |otherwise = generateL j * fromJust (eI n i) 0 - nIntegrate256 f lowerBound upperBound 
    where 
        f x = fromJust(eI' n i) x  * fromJust(eI' n j) x