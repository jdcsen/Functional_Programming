module SerDesTest where
import Test.HUnit
import SantoriniRep
import TestBoards
import SantoriniIO

-- Top level test case.
serDesTests = TestList [TestLabel "JSON Buffer Tests" jsonBufferTests,
                       TestLabel "Serialization and Deserialization Tests" serDeserTests]

-- JSON Buffer Tests
jsonBufferTests = TestList [TestLabel "BufEmpty" bufEmpty,
                            TestLabel "BufEmptyLong" bufEmptyLong,
                            TestLabel "CanExtract" canExtractSq]

bufEmpty = TestCase (assertEqual
                     "Fillbuffer with an invalid character is NoBuffer"
                     NoBuffer
                     (putBuffer NoBuffer 'a'))

bufEmptyLong = TestCase (assertEqual
                         "Fillbuffer with an invalid character is NoBuffer (sequence)"
                         [NoBuffer, NoBuffer, NoBuffer, NoBuffer, NoBuffer]
                         -- NOTE: There are five buffers above! Not 4!
                         (scanl putBuffer NoBuffer "abcd"))

canExtractSq = TestCase (assertEqual
                         "Tests to verify we can extract a square bracket from a string (square brackets"
                         [SaturatedBuffer "[1234]"]
                         ((filter isSaturated .
                           scanl putBuffer NoBuffer) "asd[1234]garbo"))

-- Serialization and deserialization tests.
serDeserTests = TestList [TestLabel "Serialization: Empty" serEmpt,
                          TestLabel "Serialization: Garbage" serGarb,
                          TestLabel "Serialization: First Player" serFp,
                          TestLabel "Serialization: Second Player" serSp,
                          TestLabel "Serialization: Provided Board" serPb,
                          TestLabel "Serialization: Empty Board" serEb,
                          TestLabel "Serialization: Clockwise Players" serCw,
                          TestLabel "Serialization: Counterclockwise Players" serCcw]

serEmpt = TestCase (assertEqual
                    "Asserts that serialization fails on the empty string."
                    Nothing
                    (deserJBoard ""))

serGarb = TestCase (assertEqual
                   "Asserts that serialization fails on a garbage string."
                   Nothing
                   (deserJBoard "garbage!"))

-- Starting State: First Player
serFp = TestCase (assertEqual
                  "Asserts that we properly deserialize a first-player starting case."
                  (Just JBoard { turn=Nothing,
                                 spaces=Nothing,
                                 players = [[JPt []]]
                              })
                  (deserJBoard "{\"players\":[[[]]]}"))

-- Starting State: Second Player
serSp = TestCase (assertEqual
                  "Asserts that we properly deserialize a second-player starting case."
                  (Just JBoard { turn=Nothing,
                                 spaces=Nothing,
                                 players = [[JPt [2,5], JPt[3,5]]]
                              })
                  (deserJBoard "{\"players\":[[[2,5],[3,5]]]}"))

-- Provided Board:
serPb = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just provJBoard)
                  (deserJBoard provBoardStr))

-- Empty Board:
serEb = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just emptJBoard)
                  (deserJBoard emptBoardStr))

-- Clockwise player position board.
serCw = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just cwiseJBoard)
                  (deserJBoard cwiseBoardStr))

-- Counterclockwise player position board.
serCcw = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just ccwiseJBoard)
                  (deserJBoard ccwiseBoardStr))
