module Main where

import SantoriniIO
import AIKernels
import qualified System.IO as SIO

main :: IO ()
main = do SIO.hSetBuffering SIO.stdin SIO.NoBuffering
          kernelRunner identityKernel
