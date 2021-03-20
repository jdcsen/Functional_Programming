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
        (HValueDefault, p1TrapIBoard, Turn [])
        (playout p1TrapIBoard (HValueDefault, HValueDefault))
    )

playoutDualHValueTest =
  TestCase
    ( assertEqual
        "Assert that the playout for two HMove kernels is as expected."
        (HValueDefault,
         IBoard
           {iturn = 80,
            ispaces = [[0,2,3,3,5],
                       [5,5,5,5,0],
                       [5,5,5,5,0],
                       [5,5,5,5,0],
                       [5,5,5,5,5]],
            iplayers =
              [IPlayer
                {icard = Artemis,
                 itokens = [IPt {row = 0, col = 0},IPt {row = 3, col = 4}]
                },
               IPlayer
                 {icard = Prometheus,
                  itokens = [IPt {row = 1, col = 4},IPt {row = 2, col = 4}]
                 }
              ]
           },Turn [])
        (playout provIBoard (HValueDefault, HValueDefault))
    )
