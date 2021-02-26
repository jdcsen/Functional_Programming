module SerDesTest where
import Test.HUnit
import SantoriniRep
import TestBoards
import SantoriniIO

-- Top level test case.
serDesTests = TestList [TestLabel "Serialization and Deserialization Tests" serDeserTests]

-- Serialization and deserialization tests.
serDeserTests = TestList [TestLabel "Serialization: Empty" serEmpt,
                          TestLabel "Serialization: Garbage" serGarb,
                          TestLabel "Serialization: First Player (CBracket Case)" serCFp,
                          TestLabel "Serialization: First Player (SBracket Case)" serSFp,
                          TestLabel "Serialization: Second Player" serSp,
                          TestLabel "Serialization: Provided Board" serPb,
                          TestLabel "Serialization: Empty Board" serEb,
                          TestLabel "Serialization: Clockwise Players" serCw,
                          TestLabel "Serialization: Counterclockwise Players" serCcw]

serEmpt = TestCase (assertEqual
                    "Asserts that deserialization fails on the empty string."
                    Nothing
                    (fromBuffer ""))

serGarb = TestCase (assertEqual
                   "Asserts that deserialization fails on a garbage string."
                   Nothing
                   (fromBuffer "garbage!"))

-- Starting State: First Player
serCFp = TestCase (assertEqual
                  "Asserts that we properly deserialize a first-player starting case (CBracket Case)."
                  (Just JBoard { turn=Nothing,
                                 spaces=Nothing,
                                 players = [[]]
                              })
                  (fromBuffer "{[]}"))

serSFp = TestCase (assertEqual
                  "Asserts that we properly deserialize a first-player starting case (SBracket Case)."
                  (Just JBoard { turn=Nothing,
                                 spaces=Nothing,
                                 players = [[]]
                              })
                  (fromBuffer "[[]]"))

-- Starting State: Second Player
serSp = TestCase (assertEqual
                  "Asserts that we properly deserialize a second-player starting case."
                  (Just JBoard { turn=Nothing,
                                 spaces=Nothing,
                                 players = [[JPt [2,5], JPt[3,5]]]
                              })
                  (fromBuffer "{[[[2,5],[3,5]]]}"))

-- Provided Board:
serPb = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just provJBoard)
                  (fromBuffer provBoardStr))

-- Empty Board:
serEb = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just emptJBoard)
                  (fromBuffer emptBoardStr))

-- Clockwise player position board.
serCw = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just cwiseJBoard)
                  (fromBuffer cwiseBoardStr))

-- Counterclockwise player position board.
serCcw = TestCase (assertEqual
                  "Asserts that we properly deserialize the board provided in the instructions."
                  (Just ccwiseJBoard)
                  (fromBuffer ccwiseBoardStr))
