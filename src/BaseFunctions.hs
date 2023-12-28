module BaseFunctions
    ( eI,
    eI',
    ) where

import Spec

n = defaultN

eI :: Int -> Maybe (Double -> Double)
eI i
    | i < 0 || i > n = Nothing
    | otherwise = Just (constructF h i)
    where
        h = range / fromIntegral n

eI' :: Int -> Maybe (Double -> Double)
eI' i
    | i < 0 || i > n = Nothing
    | otherwise = Just (constructF' h i)
    where
        h = range / fromIntegral n

constructF :: Double -> Int -> Double -> Double
constructF h i x
    |x <= x_prev || x >= x_succ = 0 
    |x < x_curr =  (x-x_prev)/h
    |otherwise = (x_succ - x)/h
    where
        x_prev = h * fromIntegral (i - 1) + lowerBound
        x_succ = h * fromIntegral (i + 1) + lowerBound
        x_curr= h * fromIntegral i + lowerBound

constructF' :: Double -> Int -> Double -> Double
constructF' h i x  
    |x <= x_prev || x >= x_succ = 0 
    |x < x_curr = 1/h
    |otherwise = -1/h
    where
        x_prev = h * fromIntegral (i - 1) + lowerBound
        x_succ = h * fromIntegral (i + 1) + lowerBound
        x_curr = h * fromIntegral i + lowerBound