module BaseFunctions
    ( eI,
    eI',
    evalF,
    unwrapF
    ) where

import Spec

eI :: Int -> Int -> Maybe (Double -> Double)
eI n i
    | i < 0 || i > n = Nothing
    | otherwise = Just (constructF h i)
    where
        h = range / fromIntegral n
eI' :: Int -> Int -> Maybe (Double -> Double)
eI' n i
    | i < 0 || i > n = Nothing
    | otherwise = Just (constructF' h i)
    where
        h = range / fromIntegral n

constructF :: Double -> Int -> Double -> Double
constructF h i x
    |x <= x_prev || x >= x_succ = 0 
    |x < x_curr = constructF' h i x * (x - x_prev)
    |otherwise = constructF' h i x * (x_succ - x)
    where
        x_prev = h * fromIntegral (i - 1) + lowerBound
        x_succ = h * fromIntegral (i + 1) + lowerBound
        x_curr = h * fromIntegral i + lowerBound

constructF' :: Double -> Int -> Double -> Double
constructF' h i x  
    |x <= x_prev || x >= x_succ = 0 
    |x < x_curr = 1/(x_curr - x_prev)
    |otherwise = 1/(x_succ - x_curr)
    where
        x_prev = h * fromIntegral (i - 1) + lowerBound
        x_succ = h * fromIntegral (i + 1) + lowerBound
        x_curr = h * fromIntegral i + lowerBound

evalF :: Maybe(Double -> Double) -> Double -> Double
evalF (Just f) x = f x
evalF Nothing _ = 0

unwrapF :: Maybe(Double -> Double) -> (Double -> Double)
unwrapF (Just f) = f
unwrapF Nothing = error "NOOOO"