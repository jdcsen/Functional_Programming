module Main where

import SantoriniIO
import AIKernels
import SantoriniLogic
import qualified System.IO as SIO

--TODO: Dynamically load kernels based on a CLI flag.
main :: IO ()
main = do kernelRunner SIO.stdin SIO.stdout kernel
  where kernel =
          predKernel
            [ (not . isFullBoard, cornerSetup),
              (isFullBoard, hmoveCard)
            ]
