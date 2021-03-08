module SerDesTest where

import Data.Either
import SantoriniIO
import SantoriniRep
import Test.HUnit
import TestBoards

-- Top level test case.
serDesTests = TestList [TestLabel "Serialization and Deserialization Tests" serDeserTests]

-- Serialization and deserialization tests.
serDeserTests =
  TestList
    [ TestLabel "Serialization: Garbage" serGarb,
      TestLabel "Serialization: First Player" serFp,
      TestLabel "Serialization: Second Player" serSp,
      TestLabel "Serialization: Provided Board" serPb,
      TestLabel "Serialization: Empty Board" serEb,
      TestLabel "Serialization: Clockwise Players" serCw,
      TestLabel "Serialization: Counterclockwise Players" serCcw
    ]

serGarb =
  TestCase
    ( assertBool
        "Asserts that deserialization fails on a garbage string."
        (isLeft $ fromBuffer "garbage!")
    )

serFp =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize a first-player starting case (SBracket Case)."
        ( Right p1JBoard )
        (fromBuffer p1BoardStr)
    )

-- Starting State: Second Player
serSp =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize a second-player starting case."
        ( Right p2JBoard )
        (fromBuffer p2BoardStr)
    )

-- Provided Board:
serPb =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize the board provided in the instructions."
        (Right provJBoard)
        (fromBuffer provBoardStr)
    )

-- Empty Board:
serEb =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize the Empty Board."
        (Right emptJBoard)
        (fromBuffer emptBoardStr)
    )

-- Clockwise player position board.
serCw =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize The Clockwise board."
        (Right cwiseJBoard)
        (fromBuffer cwiseBoardStr)
    )

-- Counterclockwise player position board.
serCcw =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize the counter-clockwise JBoard."
        (Right ccwiseJBoard)
        (fromBuffer ccwiseBoardStr)
    )
