module KernelTest where

import AIKernels
import Data.Either
import SantoriniRep
import Test.HUnit
import TestBoards

-- Top level test case.
kernelTests = TestList [TestLabel "Scorched Earth Kernel Tests" kernelTests]

-- Scorched Earth is a deterministic kernel, that helps us verify the kernel machinery
scorchedEarthTests =
  TestList
    [ TestLabel "Scorched Earth, Move 1" seM1,
      TestLabel "Scorched Earth, Move 2" seM2,
      TestLabel "Scorched Earth, Move 3" seM3,
      TestLabel "Scorched Earth, Move 4" seM4,
      TestLabel "Scorched Earth, Move 5" seM4
    ]

-- TODO: Check sequence against sequence, instead of single run checks.

-- A series of board states, returned from the Scorched Earth kernel on subsequent moves.
-- Note: Turn number not incremented inside the kernel. Done by kernelrunner, just like player flipping.
seM0B = cwPlayersIBoard4

seM1B =
  ( IBoard
      { iturn = -1,
        ispaces =
          [ [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [1, 0, 0, 0, 0]
          ],
        iplayers =
          [ [IPt 3 0, IPt 4 4],
            [IPt 0 4, IPt 0 0]
          ]
      }
  )

seM2B =
  ( IBoard
      { iturn = -1,
        ispaces =
          [ [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0]
          ],
        iplayers =
          [ [IPt 2 0, IPt 4 4],
            [IPt 0 4, IPt 0 0]
          ]
      }
  )

seM3B =
  ( IBoard
      { iturn = -1,
        ispaces =
          [ [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0]
          ],
        iplayers =
          [ [IPt 1 0, IPt 4 4],
            [IPt 0 4, IPt 0 0]
          ]
      }
  )

seM4B =
  ( IBoard
      { iturn = -1,
        ispaces =
          [ [0, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0]
          ],
        iplayers =
          [ [IPt 0 1, IPt 4 4],
            [IPt 0 4, IPt 0 0]
          ]
      }
  )

seM5B =
  ( IBoard
      { iturn = -1,
        ispaces =
          [ [0, 1, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0],
            [1, 0, 0, 0, 0]
          ],
        iplayers =
          [ [IPt 0 2, IPt 4 4],
            [IPt 0 4, IPt 0 0]
          ]
      }
  )

-- Verify Scorched Earth, Move 1
seM1 =
  TestCase
    ( assertEqual
        "Asserts that the first move of the scorched earth kernel proceeds as expected."
        seM1B
        (scorchedEarth seM0B)
    )

-- Verify Scorched Earth, Move 2
seM2 =
  TestCase
    ( assertEqual
        "Asserts that the second move of the scorched earth kernel proceeds as expected."
        seM2B
        (scorchedEarth seM1B)
    )

-- Verify Scorched Earth, Move 3
seM3 =
  TestCase
    ( assertEqual
        "Asserts that the third move of the scorched earth kernel proceeds as expected."
        seM3B
        (scorchedEarth seM2B)
    )

-- Verify Scorched Earth, Move 4
seM4 =
  TestCase
    ( assertEqual
        "Asserts that the fourth move of the scorched earth kernel proceeds as expected."
        seM4B
        (scorchedEarth seM3B)
    )

-- Verify Scorched Earth, Move 5
seM5 =
  TestCase
    ( assertEqual
        "Asserts that the fourth move of the scorched earth kernel proceeds as expected."
        seM5B
        (scorchedEarth seM4B)
    )

