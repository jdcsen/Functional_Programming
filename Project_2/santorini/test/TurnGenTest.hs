
module TurnGenTest where

import Test.HUnit
import SantoriniRep
import TestBoards
import Turns
import TurnGenerators
import qualified Data.Set as S

-- Top level test case.
turnGenTests =
  TestList
    [ TestLabel "Tests for action 'Gen' methods" actionGenTests,
      TestLabel "genAgentAction Tests" genAgentTests,
      TestLabel "genAgentTurn Tests" genAgentTurnTests,
      TestLabel "baseGen Tests" baseGenTests
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
      TestLabel "genAgentTurn Push Test (Empty)"  genAgentTurnPushEmptTest
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

-- Generic turn generation tests.
-- Note: The functions implementing these actions are tested extensively, so testing
--       here will be minimal.
baseGenTests =
  TestList
    [ TestLabel "Trap Test" trapTest,
      TestLabel "Single Move Test" singleMoveTest
    ]

trapTest =
  TestCase
    ( assertEqual
        "Assert that, if our players are trapped, we get no moves."
        S.empty
        (baseGen trapIBoard)
    )

singleMoveTest =
  TestCase
    ( assertEqual
        "Assert that, if our players have a single possible move, we take it. "
        (S.fromList [Turn [Move (IPt 0 0) (IPt 0 1), Build (IPt 0 0)]])
        (baseGen singleMoveIBoard)
    )
