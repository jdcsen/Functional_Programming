module Main where

import SantoriniIO
import AIKernels
import SantoriniLogic
import qualified System.IO as SIO

main :: IO ()
main = do SIO.hSetBuffering SIO.stdin SIO.NoBuffering
          kernelRunner kernel
  where --TODO: Dynamically load kernels based on a CLI flag.
        kernel = predKernel [(not . isFullBoard, cornerSetup),
                             (isFullBoard, scorchedEarth)]
