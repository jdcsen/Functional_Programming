module Main (main) where
import Test.HUnit
import SantoriniIO

main :: IO Counts
main = do runTestTT jsonBufferTests

-- JSON Buffer Tests
jsonBufferTests = TestList [TestLabel "BufEmpty" bufEmpty,
                            TestLabel "BufEmptyLong" bufEmptyLong,
                            TestLabel "CanExtract" canExtractSq]

bufEmpty = TestCase (assertEqual
                     "Fillbuffer with an invalid character is NoBuffer"
                     NoBuffer
                     (fillBuffer NoBuffer 'a'))

bufEmptyLong = TestCase (assertEqual
                         "Fillbuffer with an invalid character is NoBuffer (sequence)"
                         [NoBuffer, NoBuffer, NoBuffer, NoBuffer]
                         (scanl fillBuffer NoBuffer "abcd"))

canExtractSq = TestCase (assertEqual
                         "Tests to verify we can extract a square bracket from a string (square brackets"
                         [SaturatedBuffer ('[',']') "1234"]
                         (scanl fillBuffer NoBuffer "asd[1234]garbo"))
