module Main where

import SantoriniIO

main :: IO ()
main = do interact printLoop
  where printLoop = show . jsonStream :: [Char] -> [Char]
