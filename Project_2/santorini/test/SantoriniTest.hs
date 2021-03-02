module Main (main) where
-- Unit Tests
import LogicTest (logicTests)
import SerDesTest (serDesTests)
import Test.HUnit

main :: IO Counts
-- TODO: Spruce this up with the correct error code so Cabal knows what's going on.
main = do
  runTestTT serDesTests
  runTestTT logicTests
