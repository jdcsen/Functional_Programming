module Main where

import SantoriniIO
import AIKernels
import SantoriniLogic
import System.Random
import qualified System.IO as SIO

--TODO: Dynamically load kernels based on a CLI flag.
main :: IO ()
main = do
  stdGen <- getStdGen
  let kernel = PredKernel [PP (not . isFullBoard, CornerSetup),
                           PP (isFullBoard, RandKernel stdGen)]
  kernelRunner SIO.stdin SIO.stdout kernel
