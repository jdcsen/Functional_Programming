module Main where

import SantoriniIO
import AIKernels
import SantoriniLogic
import qualified System.IO as SIO

main :: IO ()
main = do kernelRunner kernel
  where --TODO: Dynamically load kernels based on a CLI flag.
        kernel = predKernel [(not . isFullBoard, cornerSetup),
                             (isFullBoard, scorchedEarth)]
