module KernTest where

import Test.HUnit
import SantoriniRep
import TestBoards
import AIKernels
import Turns

-- Top level test case.
kernTests =
  TestList
    [ TestLabel "playout Tests" playoutTests
    ]

-- Single action tests
-- Note: The functions implementing these actions are tested extensively, so testing
--       here will be minimal.
playoutTests =
  TestList
    [ TestLabel "Playouts with trapped boards are the identity." playoutIdent,
      TestLabel "Playout with two HValueDefaults" playoutDualHValueTest
    ]

playoutIdent =
  TestCase
    ( assertEqual
        "Assert that a playout of a trapped board returns an empty turn."
        (p1TrapIBoard, Turn [])
        (playout p1TrapIBoard (HValueDefault, HValueDefault))
    )

playoutDualHValueTest =
  TestCase
    ( assertEqual
        "Assert that the playout for two HMove kernels is as expected."
        (IBoard
          {iturn = 36,
           ispaces = [[0,0,0,0,2],
                      [1,1,2,0,0],
                      [1,0,0,3,0],
                      [2,5,5,5,0],
                      [0,5,0,5,5]],
           iplayers =
             [IPlayer
               {icard = Artemis,
                itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 2}]
               },
             IPlayer
               {icard = Prometheus,
                itokens = [IPt {row = 1, col = 4},IPt {row = 2, col = 4}]
               }
             ]
          },
          Turn [])
        (playout provIBoard (HValueDefault, HValueDefault))
    )
