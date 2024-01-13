module MatrixGen(approxU)where
  
import BaseFunctions
import Spec
import Math.GaussianQuadratureIntegration(nIntegrate256)
import Data.Maybe (fromJust)

type Vector = [Double]
type Row = [Double]
type Matrix = [Row]


approxU :: Double -> Double
approxU x = sum . zipWith (*) coefficients $ basisF x
  where
    basisF x = [fromJust (eI i) x | i <- [0..n-1]]

coefficients :: [Double]
coefficients = gaussEliminationFromMatrix $ augmentMatrix generateBs generateLs

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

generateBs :: Matrix
generateBs = [ [ generateB i j | i <- [0..n-1] ] | j <- [0..n-1] ]

generateB :: Int -> Int -> Double
generateB i j  
    |abs(i - j) > 1 = 0
    |otherwise = fromJust (eI i) 0 * fromJust (eI j) 0 - nIntegrate256 f up low
    where 
        f x = fromJust(eI' i) x  * fromJust(eI' j) x * k x
        (up, low) = bounds i j

bounds :: Int -> Int -> (Double, Double)
bounds i j  | abs(i - j) == 1 = (range * fromIntegral low/fromIntegral n, (fromIntegral up*range)/fromIntegral n)
            |otherwise = (lowerBound + range * max 0.0 (fromIntegral (i - 1))/fromIntegral n, lowerBound + range * min 1.0 (fromIntegral(i+1)/fromIntegral n))
  where
    up = max i j
    low = min i j

augmentMatrix :: Matrix -> Vector -> Matrix
augmentMatrix matL vec = [(matL !! s) ++ [vec !! s]| s <-[0..length vec -1]]

-- Gauss Elimination: Solve matrix equation Ax = B
gaussEliminationFromEquation :: Matrix -> Matrix -> Vector
gaussEliminationFromEquation a b = gaussEliminationFromMatrix $ zipMatrix a b

-- Create augmented matrix from A and B
zipMatrix :: Matrix -> Matrix -> Matrix
zipMatrix [] [] = []
zipMatrix (x:xs) (y:ys) = (x ++ y) : zipMatrix xs ys

-- Gauss Elimination: Solve a given augmented matrix
gaussEliminationFromMatrix :: Matrix -> Vector
gaussEliminationFromMatrix matrix = traceBack $ gaussReduction matrix

-- Compute the row-reduced-echelon form of the matrix
gaussReduction :: Matrix -> Matrix
gaussReduction [] = []
gaussReduction matrix = r: gaussReduction rs
    where
        (r:rows) = pivotCheck matrix
        rs = map reduceRow rows
        -- Row reduction using row operations
        reduceRow row
            | head row == 0 = drop 1 row
            | otherwise = drop 1 $ zipWith (-) (map (*frac) row) r
            where
                frac = head r/head row

-- Check and swap row if pivot element is zero
pivotCheck :: Matrix -> Matrix
pivotCheck (r:rs)
    | head r /= 0 = r:rs
    | otherwise = pivotCheck (rs ++ [r])

{- Reverse the rows and columns to make the calculation easier and undo
-- the column reversion before returning the solutions
-}
traceBack :: Matrix -> Vector
traceBack = reverse . traceBack' . reverse . map reverse

-- Use back substitution to calculate the solutions
traceBack' :: Matrix -> Vector
traceBack' [] = []
traceBack' (r:rows) = var : traceBack' rs
    where
        var = head r/last r
        rs = map substituteVariable rows
        substituteVariable (x:(y:ys)) = (x-var*y):ys