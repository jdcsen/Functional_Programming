
module TurnGenTest where

import qualified Data.Set       as S
import           SantoriniRep
import           Test.HUnit
import           TestBoards
import           TurnGenerators
import           Turns

-- Top level test case.
turnGenTests =
  TestList
    [ TestLabel "Tests for action 'Gen' methods" actionGenTests,
      TestLabel "genAgentAction Tests" genAgentTests,
      TestLabel "genAgentTurn Tests" genAgentTurnTests,
      TestLabel "genAgentTurn' Tests" genAgentTurn'Tests,
      TestLabel "genAgentTurns Tests" genAgentTurnsTests,
      TestLabel "actionEGen Tests" actionEGenTests,
      TestLabel "baseGen Tests" baseGenTests,
      TestLabel "genMoves Tests" genMovesTests,
      TestLabel "hasMoves Tests" hasMovesTests
    ]

-- Tests of the basic move generation functions.
actionGenTests =
  TestList
    [ TestLabel "Single Place Test" singlePlaceTest,
      TestLabel "Double Place Test" doublePlaceTest,
      TestLabel "Build Test" buildTest,
      TestLabel "Cap Test"   capTest,
      TestLabel "Move Test"  moveTest,
      TestLabel "Swap Test"  swapTest,
      TestLabel "Push Test"  pushTest
    ]

singlePlaceTest =
  TestCase
    ( assertEqual
        "Assert that we generate a single place move. "
        [ (Place (IPt {row = 0, col = 0}),
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight]],
                  iplayers =
                    [ IPlayer {icard = Pan, itokens = [IPt {row = 0, col = 0}]},
                      IPlayer {icard = Artemis, itokens = []}
                    ]
                }
            )
          )
        ]
        (placeGen singlePlaceIBoard)
    )

doublePlaceTest =
  TestCase
    ( assertEqual
        "Assert that we generate multiple place moves."
        [ (Place (IPt {row = 0, col = 0}),
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           gIWallHeight,gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight]],
                  iplayers =
                    [ IPlayer {icard = Pan, itokens = [IPt {row = 0, col = 0}]},
                      IPlayer {icard = Artemis, itokens = []}
                    ]
                }
            )
          ),
          (Place (IPt {row = 0, col = 4}),
            ( IPt {row = 0, col = 4},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           gIWallHeight,gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight]],
                  iplayers =
                    [ IPlayer {icard = Pan, itokens = [IPt {row = 0, col = 4}]},
                      IPlayer {icard = Artemis, itokens = []}
                    ]
                }
            )
          )
        ]
        (placeGen doublePlaceIBoard)
    )

buildTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves. "
        [ (Build (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           1,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 0}, IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0}, IPt {row = 4, col = 4}]}
                    ]
                }
            )
          )
        ]
        (buildGen (IPt 0 0, singleMoveIBoard))
    )

capTest =
  TestCase
    ( assertEqual
        "Assert that we generate cap moves. "
        [ (Cap (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           gIWallHeight,gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 0},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (capGen (IPt 0 0, singleMoveIBoard))
    )

moveTest =
  TestCase
    ( assertEqual
        "Assert that we generate move moves. "
        [ (Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (moveGen (IPt 0 0, singleMoveIBoard))
    )

swapTest =
  TestCase
    ( assertEqual
        "Assert that we generate swap moves."
        [ (Swap (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 0, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (swapGen (IPt 0 0, singleSwapIBoard))
    )

pushTest =
  TestCase
    ( assertEqual
        "Assert that we generate push moves. "
        [ (Push (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           0,           gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 0, col = 2},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (pushGen (IPt 0 0, singlePushIBoard))
    )


-- Tests for the genAgentAction method, just the individual tests using our genAgentAction
-- wrapper.
genAgentTests =
  TestList
    [ TestLabel "genAgentAction Build Test" genAgentBuildTest,
      TestLabel "genAgentAction Cap Test"   genAgentCapTest,
      TestLabel "genAgentAction Move Test"  genAgentMoveTest,
      TestLabel "genAgentAction Swap Test"  genAgentSwapTest,
      TestLabel "genAgentAction Push Test"  genAgentPushTest
    ]

genAgentBuildTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves with genAgentAction."
        [ (Build (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           1,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 0}, IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0}, IPt {row = 4, col = 4}]}
                    ]
                }
            )
          )
        ]
        (genAgentAction BuildE (IPt 0 0, singleMoveIBoard))
    )

genAgentCapTest =
  TestCase
    ( assertEqual
        "Assert that we generate cap moves with genAgentAction."
        [ (Cap (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           gIWallHeight,gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 0},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentAction CapE (IPt 0 0, singleMoveIBoard))
    )

genAgentMoveTest =
  TestCase
    ( assertEqual
        "Assert that we generate move moves with genAgentAction."
        [ (Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentAction MoveE (IPt 0 0, singleMoveIBoard))
    )

genAgentSwapTest =
  TestCase
    ( assertEqual
        "Assert that we generate swap moves with genAgentAction."
        [ (Swap (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 0, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentAction SwapE (IPt 0 0, singleSwapIBoard))
    )

genAgentPushTest =
  TestCase
    ( assertEqual
        "Assert that we generate push moves with genAgentAction."
        [ (Push (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           0,           gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 0, col = 2},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentAction PushE (IPt 0 0, singlePushIBoard))
    )

-- Tests for the genAgentTurn method, just the individual tests using our genAgentAction
-- wrapper.
genAgentTurnTests =
  TestList
    [ TestLabel "genAgentTurn Build Test (Empty)" genAgentTurnBuildEmptTest,
      TestLabel "genAgentTurn Cap Test (Empty)"   genAgentTurnCapEmptTest,
      TestLabel "genAgentTurn Move Test (Empty)"  genAgentTurnMoveEmptTest,
      TestLabel "genAgentTurn Swap Test (Empty)"  genAgentTurnSwapEmptTest,
      TestLabel "genAgentTurn Push Test (Empty)"  genAgentTurnPushEmptTest,
      TestLabel "genAgentTurn Build Test (Move Base)" genAgentTurnBuildMoveTest
    ]

genAgentTurnBuildEmptTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves with genAgentTurn (empty case)."
        [ (Turn [Build (IPt {row = 0, col = 1})],
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           1,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 0}, IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0}, IPt {row = 4, col = 4}]}
                    ]
                }
            )
          )
        ]
        (genAgentTurn BuildE (Turn [], (IPt 0 0, singleMoveIBoard)))
    )

genAgentTurnCapEmptTest =
  TestCase
    ( assertEqual
        "Assert that we generate cap moves with genAgentTurn (empty case)."
        [ (Turn [Cap (IPt {row = 0, col = 1})],
            ( IPt {row = 0, col = 0},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           gIWallHeight,gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 0},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentTurn CapE (Turn [], (IPt 0 0, singleMoveIBoard)))
    )

genAgentTurnMoveEmptTest =
  TestCase
    ( assertEqual
        "Assert that we generate move moves with genAgentTurn (empty case)."
        [ (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1})],
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentTurn MoveE (Turn [], (IPt 0 0, singleMoveIBoard)))
    )

genAgentTurnSwapEmptTest =
  TestCase
    ( assertEqual
        "Assert that we generate swap moves with genAgentTurn (empty case)."
        [ (Turn [Swap (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1})],
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           gIWallHeight,gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 0, col = 0},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentTurn SwapE (Turn [], (IPt 0 0, singleSwapIBoard)))
    )

genAgentTurnPushEmptTest =
  TestCase
    ( assertEqual
        "Assert that we generate push moves with genAgentTurn (empty case)."
        [ (Turn [Push (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1})],
            ( IPt {row = 0, col = 1},
              IBoard
                { iturn = -1,
                  ispaces = [[0,           0,           0,           gIWallHeight,0],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                             [0,           gIWallHeight,gIWallHeight,gIWallHeight,0]],
                  iplayers =
                    [ IPlayer
                        { icard = Pan,
                          itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                      IPlayer
                        { icard = Artemis,
                          itokens = [IPt {row = 0, col = 2},IPt {row = 4, col = 4}]
                        }
                    ]
                }
            )
          )
        ]
        (genAgentTurn PushE (Turn [], (IPt 0 0, singlePushIBoard)))
    )

gatMoveTurn = Turn [Move (IPt 0 0) (IPt 0 1)]
gatMoveBrd  = mut winChannelIBoard gatMoveTurn
genAgentTurnBuildMoveTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves with genAgentTurn (Single move base case)."
        [ (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 0, col = 0})],
          ( IPt {row = 0, col = 1},
            IBoard
              { iturn = -1,
                ispaces = [[           1,           1,2,           1,           0],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [           0,           1,2,           1,           0]],
                iplayers =
                  [ IPlayer
                      { icard = Pan,
                        itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                      },
                    IPlayer
                      { icard = Artemis,
                        itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                      }
                  ]
              }
          )
          ),
          (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 0, col = 2})],
           (IPt {row = 0, col = 1},
            IBoard
              { iturn = -1,
                ispaces = [[           0,           1,3,           1,           0],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [           0,           1,2,           1,           0]],
                iplayers =
                  [ IPlayer
                      { icard = Pan,
                        itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                      },
                    IPlayer
                      { icard = Artemis,
                        itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                      }
                  ]
              }
           )
          ),
          (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 1, col = 2})],
           ( IPt {row = 0, col = 1},
             IBoard
               { iturn = -1,
                 ispaces = [[           0,           1,           2,           1,           0],
                            [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                            [gIWallHeight,gIWallHeight,           3,gIWallHeight,gIWallHeight],
                            [gIWallHeight,gIWallHeight,           3,gIWallHeight,gIWallHeight],
                            [           0,           1,           2,           1,           0]],
                 iplayers =
                   [ IPlayer
                       { icard = Pan,
                         itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                       },
                     IPlayer
                       { icard = Artemis,
                         itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                       }
                   ]
               }
           )
          )
        ]
        (genAgentTurn BuildE (gatMoveTurn, (IPt 0 1, gatMoveBrd)))
    )

-- Tests for the genAgentTurn` method, to verify we concatMap properly.
-- wrapper.
genAgentTurn'Tests =
  TestList
    [ TestLabel "genAgentTurn' Build Test (Single Move Base)" genAgentTurn'SingleMoveTest,
      TestLabel "genAgentTurn' Build Test (Double Move Base)" genAgentTurn'DoubleMoveTest
    ]

genAgentTurn'SingleMoveTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves with genAgentTurn' (Single move base case)."
        [ (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 0, col = 0})],
          ( IPt {row = 0, col = 1},
            IBoard
              { iturn = -1,
                ispaces = [[           1,           1,2,           1,           0],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [           0,           1,2,           1,           0]],
                iplayers =
                  [ IPlayer
                      { icard = Pan,
                        itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                      },
                    IPlayer
                      { icard = Artemis,
                        itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                      }
                  ]
              }
          )
          ),
          (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 0, col = 2})],
           (IPt {row = 0, col = 1},
            IBoard
              { iturn = -1,
                ispaces = [[           0,           1,3,           1,           0],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [           0,           1,2,           1,           0]],
                iplayers =
                  [ IPlayer
                      { icard = Pan,
                        itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                      },
                    IPlayer
                      { icard = Artemis,
                        itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                      }
                  ]
              }
           )
          ),
          (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 1, col = 2})],
           ( IPt {row = 0, col = 1},
             IBoard
               { iturn = -1,
                 ispaces = [[           0,           1,           2,           1,           0],
                            [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                            [gIWallHeight,gIWallHeight,           3,gIWallHeight,gIWallHeight],
                            [gIWallHeight,gIWallHeight,           3,gIWallHeight,gIWallHeight],
                            [           0,           1,           2,           1,           0]],
                 iplayers =
                   [ IPlayer
                       { icard = Pan,
                         itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                       },
                     IPlayer
                       { icard = Artemis,
                         itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                       }
                   ]
               }
           )
          )
        ]
        (genAgentTurn' [(gatMoveTurn, (IPt 0 1, gatMoveBrd))] BuildE)
    )

secondMoveTurn = Turn [Move (IPt 0 4) (IPt 0 3)]
secondMoveBrd  = mut winChannelIBoard secondMoveTurn
genAgentTurn'DoubleMoveTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves with genAgentTurn' (Single move base case)."
        [ (Turn
            [ Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
              Build (IPt {row = 0, col = 0})
            ],
           (IPt {row = 0, col = 1},
            IBoard
              {iturn = -1,
               ispaces = [[1,1,2,1,0],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [0,1,2,1,0]],
               iplayers =
                 [IPlayer
                    {icard = Pan,
                     itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                    },
                  IPlayer
                    {icard = Artemis,
                     itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                    }
                 ]
              }
           )
          ),
          (Turn
            [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
             Build (IPt {row = 0, col = 2})],
           (IPt {row = 0, col = 1},
            IBoard
              {iturn = -1,
               ispaces = [[0,1,3,1,0],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [0,1,2,1,0]],
               iplayers =
                 [ IPlayer
                     { icard = Pan,
                       itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                   IPlayer
                     { icard = Artemis,
                       itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                     }
                 ]
              }
           )
          ),
          (Turn
            [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
             Build (IPt {row = 1, col = 2})],
           (IPt {row = 0, col = 1},
            IBoard
              {iturn = -1,
               ispaces = [[0,1,2,1,0],
                          [5,5,5,5,5],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [0,1,2,1,0]],
               iplayers =
                 [ IPlayer
                     { icard = Pan,
                       itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]},
                   IPlayer
                     { icard = Artemis,
                       itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                     }
                 ]
              }
           )
          ),
          (Turn
            [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),
             Build (IPt {row = 0, col = 2})],
           (IPt {row = 0, col = 3},
            IBoard
              {iturn = -1,
               ispaces = [[0,1,3,1,0],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [0,1,2,1,0]],
               iplayers =
                 [ IPlayer
                     { icard = Pan,
                       itokens = [IPt {row = 0, col = 3},IPt {row = 0, col = 0}]},
                   IPlayer
                     { icard = Artemis,
                       itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                     }
                 ]
              }
           )
          ),
          (Turn
            [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),
             Build (IPt {row = 0, col = 4})],
           (IPt {row = 0, col = 3},
            IBoard
              {iturn = -1,
               ispaces = [[0,1,2,1,1],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [0,1,2,1,0]],
               iplayers =
                 [ IPlayer
                     { icard = Pan,
                       itokens = [IPt {row = 0, col = 3},IPt {row = 0, col = 0}]},
                   IPlayer
                     { icard = Artemis,
                       itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                     }
                 ]
              }
           )
          ),
          (Turn
            [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),
             Build (IPt {row = 1, col = 2})],
           (IPt {row = 0, col = 3},
            IBoard
              {iturn = -1,
               ispaces = [[0,1,2,1,0],
                          [5,5,5,5,5],
                          [5,5,3,5,5],
                          [5,5,3,5,5],
                          [0,1,2,1,0]],
               iplayers =
                 [ IPlayer
                     { icard = Pan,
                       itokens = [IPt {row = 0, col = 3},IPt {row = 0, col = 0}]},
                   IPlayer
                     { icard = Artemis,
                       itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                     }
                 ]
              }
           )
          )
        ]
        (genAgentTurn' [(gatMoveTurn, (IPt 0 1, gatMoveBrd)),
                        (secondMoveTurn, (IPt 0 3, secondMoveBrd))] BuildE)
    )

genAgentTurnsTests =
  TestList
    [ TestLabel "genAgentTurn' Build Test (Single Move Base)" genAgentTurnsMoveTest
    ]

genAgentTurnsMoveTest =
  TestCase
    ( assertEqual
        "Assert that we generate build moves with genAgentTurns (Move and Build case)."
        [ (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 0, col = 0})],
          ( IPt {row = 0, col = 1},
            IBoard
              { iturn = -1,
                ispaces = [[           1,           1,2,           1,           0],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [           0,           1,2,           1,           0]],
                iplayers =
                  [ IPlayer
                      { icard = Pan,
                        itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                      },
                    IPlayer
                      { icard = Artemis,
                        itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                      }
                  ]
              }
          )
          ),
          (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 0, col = 2})],
           (IPt {row = 0, col = 1},
            IBoard
              { iturn = -1,
                ispaces = [[           0,           1,3,           1,           0],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [gIWallHeight,gIWallHeight,3,gIWallHeight,gIWallHeight],
                           [           0,           1,2,           1,           0]],
                iplayers =
                  [ IPlayer
                      { icard = Pan,
                        itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                      },
                    IPlayer
                      { icard = Artemis,
                        itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                      }
                  ]
              }
           )
          ),
          (Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),
                 Build (IPt {row = 1, col = 2})],
           ( IPt {row = 0, col = 1},
             IBoard
               { iturn = -1,
                 ispaces = [[           0,           1,           2,           1,           0],
                            [gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight,gIWallHeight],
                            [gIWallHeight,gIWallHeight,           3,gIWallHeight,gIWallHeight],
                            [gIWallHeight,gIWallHeight,           3,gIWallHeight,gIWallHeight],
                            [           0,           1,           2,           1,           0]],
                 iplayers =
                   [ IPlayer
                       { icard = Pan,
                         itokens = [IPt {row = 0, col = 1},IPt {row = 0, col = 4}]
                       },
                     IPlayer
                       { icard = Artemis,
                         itokens = [IPt {row = 4, col = 0},IPt {row = 4, col = 4}]
                       }
                   ]
               }
           )
          )
        ]
        (genAgentTurns [MoveE, BuildE] (IPt 0 0, winChannelIBoard))
    )

-- ActionEGen tests
actionEGenTests =
  TestList
    [ TestLabel "Pan BAM Test" trapTest,
      TestLabel "Apollo BAM Test" singleMoveTest
    ]

panBAMTest =
  TestCase
    ( assertEqual
        "Assert that we trim builds after moves with actionEGen and Pan"
        []
        (actionEGen Pan apolloBuildAfterWinIBoard [MoveE, BuildE] (const True))
    )

apolloBAMTest =
  TestCase
    ( assertEqual
        "Assert that we trim builds after moves with actionEGen and Pan"
        []
        (actionEGen Apollo apolloBuildAfterWinIBoard [SwapE, BuildE] (const True))
    )

-- Generic turn generation tests.
baseGenTests =
  TestList
    [ TestLabel "Trap Test" trapTest,
      TestLabel "Single Move Test" singleMoveTest,
      TestLabel "Win Channel Test" winChannelTest,
      TestLabel "Wide Open Test" wideOpenTest
    ]

trapTest =
  TestCase
    ( assertEqual
        "Assert that, if our players are trapped, we get no moves."
        []
        (baseGen trapIBoard)
    )

singleMoveTest =
  TestCase
    ( assertEqual
        "Assert that, if our players have a single possible move, we take it. "
        [Turn [Move (IPt 0 0) (IPt 0 1), Build (IPt 0 0)]]
        (baseGen singleMoveIBoard)
    )


winChannelTest =
  TestCase
    ( assertEqual
        "Assert that, if we have a winChannel starting case with players, we get the proper TurnSet from it."
        [Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),Build (IPt {row = 0, col = 0})],
         Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),Build (IPt {row = 0, col = 2})],
         Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),Build (IPt {row = 1, col = 2})],
         Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),Build (IPt {row = 0, col = 2})],
         Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),Build (IPt {row = 0, col = 4})],
         Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),Build (IPt {row = 1, col = 2})]]
        (baseGen winChannelIBoard)
    )

wideOpenTest =
  TestCase
    ( assertEqual
        "Assert that, if we have a wide-open starting case with players in the corners, we get the proper TurnSet from it."
        (S.fromList
          [Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),Build (IPt {row = 0, col = 2})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 0, col = 3}),Build (IPt {row = 0, col = 4})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 3}),Build (IPt {row = 0, col = 2})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 3}),Build (IPt {row = 0, col = 3})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 4}),Build (IPt {row = 0, col = 3})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 4}),Build (IPt {row = 0, col = 4})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 3}),Build (IPt {row = 1, col = 2})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 4}),Build (IPt {row = 1, col = 3})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 3}),Build (IPt {row = 2, col = 2})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 3}),Build (IPt {row = 2, col = 3})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 4}),Build (IPt {row = 2, col = 3})],
           Turn [Move (IPt {row = 0, col = 4}) (IPt {row = 1, col = 4}),Build (IPt {row = 2, col = 4})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 0}),Build (IPt {row = 2, col = 0})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 1}),Build (IPt {row = 2, col = 0})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 1}),Build (IPt {row = 2, col = 1})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 1}),Build (IPt {row = 2, col = 2})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 1}),Build (IPt {row = 3, col = 0})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 1}),Build (IPt {row = 3, col = 2})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 3, col = 0}),Build (IPt {row = 4, col = 0})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 4, col = 1}),Build (IPt {row = 3, col = 0})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 4, col = 1}),Build (IPt {row = 3, col = 1})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 4, col = 1}),Build (IPt {row = 3, col = 2})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 4, col = 1}),Build (IPt {row = 4, col = 0})],
           Turn [Move (IPt {row = 4, col = 0}) (IPt {row = 4, col = 1}),Build (IPt {row = 4, col = 2})]])
        (S.fromList $ baseGen ccwiseIBoard)
    )

-- Card Specific turn generation tests.
genMovesTests =
  TestList
    [ TestLabel "Apollo-endgame (Integration Test)" apolloEndgameTest
    ]

apolloEndgameTest =
  TestCase
    ( assertEqual
        "Assert that, if we have a wide-open starting case with players in the corners, we get the proper TurnSet from it."
        (S.fromList
          [Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),Build (IPt {row = 0, col = 0})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 0, col = 1}),Build (IPt {row = 0, col = 2})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 0}),Build (IPt {row = 0, col = 0})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 0, col = 0})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 0, col = 1})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 0, col = 2})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 1, col = 0})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 1, col = 2})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 0}),Build (IPt {row = 2, col = 0})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 2, col = 0})],
           Turn [Move (IPt {row = 0, col = 0}) (IPt {row = 1, col = 1}),Build (IPt {row = 2, col = 1})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 4, col = 4})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 2}),Build (IPt {row = 2, col = 1})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 2}),Build (IPt {row = 2, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 3}),Build (IPt {row = 2, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 4}),Build (IPt {row = 2, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 4}),Build (IPt {row = 2, col = 4})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 2}),Build (IPt {row = 3, col = 1})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 3}),Build (IPt {row = 3, col = 2})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 4}),Build (IPt {row = 3, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 4, col = 2}),Build (IPt {row = 3, col = 1})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 4, col = 2}),Build (IPt {row = 3, col = 2})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 4, col = 2}),Build (IPt {row = 3, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 3}),Build (IPt {row = 4, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 4}),Build (IPt {row = 4, col = 3})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 3, col = 4}),Build (IPt {row = 4, col = 4})],
           Turn [Move (IPt {row = 4, col = 3}) (IPt {row = 4, col = 2}),Build (IPt {row = 4, col = 3})]])
        (genMoves Apollo apolloBuildAfterWinIBoard)
    )

hasMovesTests =
  TestList
    [ TestLabel "hasMoves Wide Open" hasMovesWideOpenTest,
      TestLabel "hasMoves Both Trapped" hasMovesBothBlockedTest,
      TestLabel "hasMoves P1 Open" hasMovesP1OpenTest,
      TestLabel "hasMoves P2 Open" hasMovesP2OpenTest
    ]

hasMovesWideOpenTest =
  TestCase
    ( assertEqual
        "Assert that, if given a wide-open board, both players will have moves."
        [True, True]
        (hasMoves cwPlayersIBoard4)
    )

hasMovesBothBlockedTest =
  TestCase
    ( assertEqual
        "Assert that, if given a board where both players are trapped, neither has moves."
        [False, False]
        (hasMoves trapIBoard)
    )

hasMovesP1OpenTest =
  TestCase
    ( assertEqual
        "Assert that, if given a board where the first player is open and the \
        \ second is trapped, we get the proper hasMoves array."
        [True, False]
        (hasMoves p2TrapIBoard)
    )

hasMovesP2OpenTest =
  TestCase
    ( assertEqual
        "Assert that, if given a board where the second player is open and the \
        \ first is trapped, we get the proper hasMoves array."
        [False, True]
        (hasMoves p1TrapIBoard)
    )
