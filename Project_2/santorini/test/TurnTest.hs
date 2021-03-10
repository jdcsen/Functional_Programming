module TurnTest where
import Test.HUnit
import SantoriniRep
import TestBoards
import Turns

-- Top level test case.
turnTests =
  TestList
    [ TestLabel "Single Action Tests" singleActionTests,
      TestLabel "Turn Tests" comboActionTests
    ]

-- Single action tests
-- Note: The functions implementing these actions are tested extensively, so testing
--       here will be minimal.
singleActionTests =
  TestList
    [ TestLabel "Place Test" placeTest,
      TestLabel "Build Test" buildTest,
      TestLabel "Move Test" moveTest,
      TestLabel "Swap Test" swapTest,
      TestLabel "Push Test" pushTest
    ]

placeTest =
  TestCase
    ( assertEqual
        "Assert that the place action can place a player"
        cwPlayersIBoard1
        (mutate p1IBoard (Place (IPt 0 0)))
    )

buildTest =
  TestCase
    ( assertEqual
        "Assert that the build action can build a tower"
        ( IBoard
            { iturn = -1,
              ispaces =
                [ [1, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ],
              iplayers = []
            }
        )
        (mutate gIBoardEmpty (Build (IPt 0 0)))
    )

moveTest =
  TestCase
    ( assertEqual
        "Assert that the move action can move a player."
        ( IBoard
            { iturn = -1,
              ispaces =
                [ [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ],
              iplayers =
                [ IPlayer { icard = Prometheus, itokens = [IPt 3 1, IPt 4 4]},
                  IPlayer { icard = Artemis,    itokens = [IPt 0 4, IPt 0 0]}
                ]
            }
        )
        (mutate cwPlayersIBoard4 (Move (IPt 4 0) (IPt 3 1)))
    )

swapTest =
  TestCase
    ( assertEqual
        "Assert that the swap action swaps players."
        player11SwapIBoard
        (mutate preSwapIBoard (Swap (IPt 4 0) (IPt 0 4)))
    )

pushTest =
  TestCase
    ( assertEqual
        "Assert that the push action pushes players."
        pushUpIBoardFlat
        (mutate prePushIBoardFlat (Push (IPt 2 2) (IPt 1 2)))
    )

comboActionTests =
  TestList
    [ TestLabel "Turn Place Test" placeTurnTest,
      TestLabel "Turn Build Test" buildTurnTest,
      TestLabel "Turn Move Test" moveTurnTest,
      TestLabel "Turn Swap Test" swapTurnTest,
      TestLabel "Turn Push Test" pushTurnTest
    ]

placeTurnTest =
  TestCase
    ( assertEqual
        "Asserts that multiple place actions can be executed in sequence."
        cwPlayersIBoard2
        (mutate
          p1IBoard
          ( Turn
            [ Place $ IPt 0 0,
              Place $ IPt 0 4
            ]
          )
        )
    )

buildTurnTest =
  TestCase
    ( assertEqual
        "Asserts that multiple build actions can be executed in sequence."
        ( IBoard
            { iturn = -1,
              ispaces =
                [ [gIWallHeight, 0, 0, 0, 0],
                  [0,            0, 0, 0, 0],
                  [0,            0, 0, 0, 0],
                  [0,            0, 0, 0, 0],
                  [0,            0, 0, 0, 0]
                ],
              iplayers =
                [ IPlayer {icard = Artemis,    itokens = []},
                  IPlayer {icard = Prometheus, itokens = []}
                ]
            }
        )
        (mutate
          p1IBoard
          ( Turn
            [ Build $ IPt 0 0,
              Build $ IPt 0 0,
              Build $ IPt 0 0,
              Build $ IPt 0 0
            ]
          )
        )
    )

moveTurnTest =
  TestCase
    ( assertEqual
        "Asserts that multiple move actions can be executed in sequence."
        ( IBoard
            { iturn = -1,
              ispaces =
                [ [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ],
              iplayers =
                [ IPlayer { icard = Prometheus, itokens = [IPt 3 3, IPt 4 4]},
                  IPlayer { icard = Artemis,    itokens = [IPt 0 4, IPt 4 0]}
                ]
            }
        )
        (mutate
          ( IBoard
              { iturn = -1,
                ispaces =
                  [ [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0]
                  ],
                iplayers =
                  [ IPlayer { icard = Prometheus, itokens = [IPt 0 0, IPt 4 4]},
                    IPlayer { icard = Artemis,    itokens = [IPt 0 4, IPt 4 0]}
                  ]
              }
          )
          ( Turn
            [ Move (IPt 0 0) (IPt 1 1),
              Move (IPt 1 1) (IPt 2 2),
              Move (IPt 2 2) (IPt 3 3)
            ]
          )
        )
    )

swapTurnTest =
  TestCase
    ( assertEqual
        "Asserts that multiple swap actions can be executed in sequence."
        ( IBoard
            { iturn = -1,
              ispaces =
                [ [0, 0, 0, 0, 1],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [3, 0, 0, 0, 2]
                ],
              iplayers =
                [ IPlayer { icard = Prometheus, itokens = [IPt 0 4, IPt 0 0]},
                  IPlayer { icard = Artemis,    itokens = [IPt 4 0, IPt 4 4]}
                ]
            }
        )
        (mutate
          preSwapIBoard
          ( Turn
            [ Swap (IPt 0 0) (IPt 4 4),
              Swap (IPt 4 0) (IPt 0 4)
            ]
          )
        )
    )

pushTurnTest =
  TestCase
    ( assertEqual
        "Asserts that multiple push actions can be executed in sequence."
        ( IBoard
            { iturn = -1,
              ispaces =
                [ [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ],
              iplayers =
                [ IPlayer { icard = Minotaur, itokens = [IPt 2 1, IPt 1 2]},
                  IPlayer { icard = Artemis,  itokens = [IPt 3 1, IPt 0 2]}
                ]
            }
        )
        (mutate
          prePushIBoardFlat
          ( Turn
            [ Push (IPt 2 2) (IPt 1 2),
              Push (IPt 1 1) (IPt 2 1)
            ]
          )
        )
    )
