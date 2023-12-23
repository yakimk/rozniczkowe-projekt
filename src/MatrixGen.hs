module MatrixGen(generateBs, generateLs)where

import BaseFunctions
import Spec
import Math.GaussianQuadratureIntegration(nIntegrate256)

n :: Int
n = defaultN

generateLs :: Int -> [Double]
generateLs n = [generateL i | i <- [0..n]]

generateL :: Int -> Double
generateL i 
    |i  == 0 = 20
    |otherwise = 0


generateBs :: Int -> [[Double]]
generateBs n = [ [ generateB i j | i <- [1..n] ] | j <- [1..n] ]

generateB :: Int -> Int -> Double
generateB i j  
    |abs(i - j) > 1 = 0
    |otherwise = generateL j * evalF (eI n i) 0 - nIntegrate256 f lowerBound upperBound 
    where 
        f x = unwrapF(eI' n i) x  * unwrapF(eI' n j) x