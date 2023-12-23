module Main (main) where

import BaseFunctions
import MatrixGen
import Data.Maybe (fromMaybe)
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
import Text.Read (readMaybe)

n = defaultN

approxPlot :: [(Double, Double)]
approxPlot = [(x i , approxU (x i)) | i <- [0..n]] :: [(Double, Double)]
    where
        h = range / fromIntegral n
        x i = h  * fromIntegral i + lowerBound
exactPlot :: [(Double, Double)]
exactPlot = [(x, u x) | x <- [lowerBound, 0.1 .. upperBound]] :: [(Double, Double)]
  where
    u x = 0.5 * (x * cos x + ((cos 2 + 2 * sin 2) * sin x) / (cos 2 - sin 2))

parseArg :: [String] -> Int
parseArg [arg] = read arg
parseArg _ = 5

main :: IO ()
main = do
  args <- getArgs
--   let n = parseArg args :: Int

  toFile def "solution.svg" $ do
    layout_title .= "-u'' - u = sinx        u(0) = 0,    u'(2) - u(2) = 0"
    setColors [opaque blue, opaque blue, opaque red]
    plot (line "Aproximate solution" [approxPlot])
    plot (points "" approxPlot)
    plot (line "Exact solution" [exactPlot])

  putStrLn "Done."