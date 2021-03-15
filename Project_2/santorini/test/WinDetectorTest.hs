module WinDetectorTest where

import Test.HUnit
import SantoriniRep
import TestBoards
import WinDetector


-- Top level test case.
winDetectorTests =
  TestList
    [ TestLabel "WinState Tests" winStateTests
    ]

-- Single action tests
-- Note: The functions implementing these actions are tested extensively, so testing
--       here will be minimal.
winStateTests =
  TestList
    [ TestLabel "Win Chain Test" winChainTest,
      TestLabel "Loss Chain Test" lossChainTest,
      TestLabel "Neither Chain Test" neitherChainTest,
      TestLabel "Win-Neither Priority Test" winNeitherPriorityTest,
      TestLabel "Loss-Neither Priority Test" lossNeitherPriorityTest,
      TestLabel "Loss-Win Priority Test" lossWinPriorityTest,
      TestLabel "Win-Loss Priority Test" winLossPriorityTest
    ]

winChainTest =
  TestCase
    ( assertEqual
        "Assert that a chain of wins equals a win"
        (Win <> Win <> Win)
        Win
    )

lossChainTest =
  TestCase
    ( assertEqual
        "Assert that a chain of losses equals a loss"
        (Loss <> Loss <> Loss)
        Loss
    )

neitherChainTest =
  TestCase
    ( assertEqual
        "Assert that a chain of neither equals a neither"
        (Neither <> Neither <> Neither)
        Neither
    )

winNeitherPriorityTest =
  TestCase
    ( assertEqual
        "Assert that a chain of neithers is overwritten by a win."
        (Neither <> Neither <> Win)
        Win
    )

lossNeitherPriorityTest =
  TestCase
    ( assertEqual
        "Assert that a chain of neithers is overwritten by a loss."
        (Neither <> Neither <> Loss)
        Loss
    )

lossWinPriorityTest =
  TestCase
    ( assertEqual
        "Assert that a chain of neithers with a win is not overwritten by a loss."
        (Neither <> Win <> Loss)
        Win
    )

winLossPriorityTest =
  TestCase
    ( assertEqual
        "Assert that a chain of neithers with a loss is not overwritten by a win."
        (Neither <> Loss <> Win)
        Loss
    )
