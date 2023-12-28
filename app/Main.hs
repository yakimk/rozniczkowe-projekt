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


approxPlot :: Int -> [(Double, Double)]
approxPlot n = [(x i , approxU (x i)) | i <- [0..n]] :: [(Double, Double)]
    where
        h = range / fromIntegral n
        x i = h  * fromIntegral i + lowerBound

parseArg :: [String] -> Int
parseArg [arg] = read arg
parseArg _ = defaultN

main :: IO ()
main = do
  args <- getArgs
  let n = parseArg args :: Int

  toFile def "solution.svg" $ do
    setColors [opaque blue, opaque blue, opaque red]
    plot (line "Aproximate solution" [approxPlot n])
    plot (points "" $ approxPlot n)

  putStrLn "Done."