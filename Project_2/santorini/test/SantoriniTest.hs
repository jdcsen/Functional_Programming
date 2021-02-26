module Main (main) where
import Test.HUnit
-- Unit Tests
import SerDesTest (serDesTests)
import LogicTest  (logicTests)

main :: IO Counts
-- TODO: Spruce this up with the correct error code so Cabal knows what's going on.
main = do runTestTT serDesTests
          runTestTT logicTests

