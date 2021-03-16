module WinDetectorTest where

import Test.HUnit
import SantoriniRep
import TestBoards
import WinDetector
import Turns


-- Top level test case.
winDetectorTests =
  TestList
    [ TestLabel "WinState Tests" winStateTests,
      TestLabel "baseIsWon Tests" baseIsWonTests,
      TestLabel "Card isWon Tests" cardIsWonTests,
      TestLabel "trimTurn Tests" trimTurnTests
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

baseIsWonTests =
  TestList
    [ TestLabel "No Win Test: Sub-level 3" noWinTest,
      TestLabel "No Win Test: Level 3"     noWinLvl3Test,
      TestLabel "Win Test"                 winLvl3Test
    ]

noWinTest =
  TestCase
    ( assertEqual
        "Assert that a move from a sub-L3 level to another sub-L3 level is not a win."
        Neither
        (baseIsWon panWinIBoard (Move (IPt 1 1) (IPt 0 1)))
    )

noWinLvl3Test =
  TestCase
    ( assertEqual
        "Assert that a move from a L3 level to another L3 level is not a win."
        Neither
        (baseIsWon panL3WinIBoard (Move (IPt 1 1) (IPt 1 0)))
    )

winLvl3Test =
  TestCase
    ( assertEqual
        "Assert that a move from a sub-L3 level to a L3 level is a win."
        Win
        (baseIsWon panWinIBoard (Move (IPt 1 1) (IPt 1 0)))
    )

-- NOTE: Because the only card whose win states differ from standard is Pan, we
--       only test Pan here.
cardIsWonTests =
  TestList
    [ TestLabel "Pan No Win Test: Sub-2 drop"         panNoWinTest,
      TestLabel "Pan No Win Test: Level 3 to Level 3" panNoWinLvl3Test,
      TestLabel "Pan Win Test: Standard"              panWinStdTest,
      TestLabel "Pan Win Test: 2 Drop"                panWin2DropTest,
      TestLabel "Pan Win Test: 3 Drop"                panWin3DropTest,
      TestLabel "Minotaur Loss Test: Pan Push"        minotaurPanLossTest
    ]

panNoWinTest =
  TestCase
    ( assertEqual
        "Assert that with pan, a drop of 1 space is not a win."
        Neither
        (isWon Pan panWinIBoard (Move (IPt 1 1) (IPt 0 1)))
    )

panNoWinLvl3Test =
  TestCase
    ( assertEqual
        "Assert that, even with Pan, a move from a L3 level to another L3 level is not a win."
        Neither
        (isWon Pan panL3WinIBoard (Move (IPt 1 1) (IPt 1 0)))
    )

panWinStdTest =
  TestCase
    ( assertEqual
        "Assert that, even with Pan, a move from a sub-L3 level to a L3 level is a win."
        Win
        (isWon Pan panWinIBoard (Move (IPt 1 1) (IPt 1 0)))
    )

panWin2DropTest =
  TestCase
    ( assertEqual
        "Assert that with Pan, a move down two levels is a win."
        Win
        (isWon Pan panWinIBoard (Move (IPt 1 1) (IPt 0 0)))
    )

panWin3DropTest =
  TestCase
    ( assertEqual
        "Assert that with Pan, a move down two levels is a win."
        Win
        (isWon Pan panL3WinIBoard (Move (IPt 1 1) (IPt 0 0)))
    )

minotaurPanLossTest =
  TestCase
    ( assertEqual
        "Assert that with Minotaur, shoving Pan down two levels is a Loss."
        Loss
        (isWon Minotaur minotaurLossIBoard (Push (IPt 3 1) (IPt 2 1)))
    )

trimTurnTests =
  TestList
    [ TestLabel "Empty Turns are unchanged" trimEmptTest,
      TestLabel "Neither Turns are unchanged" trimNeitherTest,
      TestLabel "Actions past wins are trimmed." trimWinTest,
      TestLabel "Actions past losses are trimmed." trimLossTest
    ]

trimEmptTest =
  TestCase
    ( assertEqual
        "Assert that trimming an empty turn list leaves the list unchanged."
        (Turn [])
        (trimTurn Pan panWinIBoard (Turn []))
    )

trimNeitherTest =
  TestCase
    ( assertEqual
        "Assert that non-winning/losing moves are unchanged."
        (Turn [Move (IPt 1 1) (IPt 0 1), Build (IPt 1 1)])
        (trimTurn Pan panWinIBoard (Turn [Move (IPt 1 1) (IPt 0 1), Build (IPt 1 1)]))
    )

-- NOTE: According to Dr. Flatt, Pan can ~technically~ build after dropping, and
--       the win is optional. Since we don't check other player's move, I've just
--       made the choice to accept the win in every case, as it seems foolish not to.
--       It kind of mixes turn decisions with win logic, but it's enough of a time
--       saver that I think I'll just accept it.
trimWinTest =
  TestCase
    ( assertEqual
        "Assert that moves after wins are trimmed."
        (Turn [Move (IPt 1 1) (IPt 0 0)])
        (trimTurn Pan panWinIBoard (Turn [Move (IPt 1 1) (IPt 0 0), Build (IPt 1 1)]))
    )

trimLossTest =
  TestCase
    ( assertEqual
        "Assert that moves after Losses are trimmed.."
        (Turn [Push (IPt 3 1) (IPt 2 1)])
        (trimTurn Minotaur minotaurLossIBoard (Turn [Push (IPt 3 1) (IPt 2 1), Build (IPt 0 0)]))
    )
