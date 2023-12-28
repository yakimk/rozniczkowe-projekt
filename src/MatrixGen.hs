module MatrixGen(generateBs, generateLs, coefficients, approxU)where

import BaseFunctions
import Spec
import Math.GaussianQuadratureIntegration(nIntegrate256)
import Data.Maybe (fromJust)
import Numeric.LinearAlgebra (flatten, fromLists, linearSolve, luSolve, toList, toLists, (><))

approxU :: Double -> Double
approxU x = sum . zipWith (*) coefficients $ basisF x
  where
    basisF x = [fromJust (eI i) x | i <- [0..n-1]]

coefficients :: [Double]
coefficients = toList . flatten $ fromJust (linearSolve b_matrix l_vector)
  where
    b_matrix = fromLists generateBs 
    l_vector = (n >< 1) generateLs

n :: Int
n = defaultN

k :: Double -> Double
k x 
  |x < lowerBound || x > upperBound = 0
  |x <= 1 = 1
  |otherwise = 2

generateLs :: [Double]
generateLs = [generateL i | i <- [0..n-1]]

generateL :: Int -> Double
generateL i 
    |i  == 0 = 20 * fromJust(eI i) 0
    |otherwise = 0

generateBs :: [[Double]]
generateBs = [ [ generateB i j | i <- [0..n-1] ] | j <- [0..n-1] ]

generateB :: Int -> Int -> Double
generateB i j  
    |abs(i - j) > 1 = 0
    |otherwise = fromJust (eI i) 0 * fromJust (eI j) 0 - nIntegrate256 f up low
    where 
        f x = fromJust(eI' i) x  * fromJust(eI' j) x * k x
        (up, low) = (0, 2)

bounds :: Int -> Int -> (Double, Double)
bounds i j  | abs(i - j) == 1 = ((fromIntegral low * range)/fromIntegral n, (fromIntegral up*range)/fromIntegral n)
            |otherwise = (lowerBound + h*fromIntegral i, lowerBound + h * fromIntegral(i+1))
  where
    up = max i j
    low = min i j
    h = range/fromIntegral n