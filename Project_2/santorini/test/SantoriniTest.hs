module Main (main) where
import Test.HUnit
-- Unit Tests
import SerDesTest (serDesTests)
import LogicTest  (logicTests)

main :: IO Counts
main = do runTestTT serDesTests
          runTestTT logicTests

