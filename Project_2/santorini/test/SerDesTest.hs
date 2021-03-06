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
      TestLabel "Serialization: First Player (Empty)" serFpE,
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

-- Starting State: First Player
serFpE =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize a first-player starting case (Empty Case)."
        ( Right gJBoardEmpty )
        (fromBuffer "")
    )

serFp =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize a first-player starting case (SBracket Case)."
        ( Right gJBoardEmpty )
        (fromBuffer "[]")
    )

-- Starting State: Second Player
serSp =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize a second-player starting case."
        ( Right
            JBoard
              { turn = Nothing,
                spaces = Nothing,
                players = [JPlayer { card = "Apollo", tokens = Just [[2, 5], [3, 5]]}]
              }
        )
        (fromBuffer "[[[2,5],[3,5]]]")
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
        "Asserts that we properly deserialize the board provided in the instructions."
        (Right emptJBoard)
        (fromBuffer emptBoardStr)
    )

-- Clockwise player position board.
serCw =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize the board provided in the instructions."
        (Right cwiseJBoard)
        (fromBuffer cwiseBoardStr)
    )

-- Counterclockwise player position board.
serCcw =
  TestCase
    ( assertEqual
        "Asserts that we properly deserialize the board provided in the instructions."
        (Right ccwiseJBoard)
        (fromBuffer ccwiseBoardStr)
    )
