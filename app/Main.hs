module Main (main) where

import BaseFunctions()
import MatrixGen
import Data.Maybe()
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy
  ( Default (def),
    blue,
    layout_title,
    line,
    opaque,
    plot,
    points,
    red,
    setColors,
    (.=),
  )
import Spec
import System.Environment (getArgs)
import System.Exit ()
import Text.Read()

n = defaultN

approxPlot :: [(Double, Double)]
approxPlot = [(x i , approxU (x i)) | i <- [0..n]] :: [(Double, Double)]
    where
        h = range / fromIntegral n
        x i = h  * fromIntegral i + lowerBound

parseArg :: [String] -> Int
parseArg [arg] = read arg
parseArg _ = 5

main :: IO ()
main = do
  toFile def "solution.svg" $ do
    setColors [opaque blue, opaque blue, opaque red]
    plot (line "Aproximate solution" [approxPlot])
    plot (points "" approxPlot)

  putStrLn "Done."