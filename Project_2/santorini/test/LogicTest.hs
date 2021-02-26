module LogicTest where
import Test.HUnit
import SantoriniRep
import TestBoards
import SantoriniLogic

-- Top level test case.
logicTests = TestList [TestLabel "Proximity Function Tests" proxTests,
                       TestLabel "isFullBoard Tests" fbTests]

-- Full Board tests
fbTests = TestList [TestLabel "Player 1 Start" fbP1,
                    TestLabel "Player 2 Start" fbP2,
                    TestLabel "Full Game"      fbFull]

fbP1 = TestCase (assertEqual
                       "Assert that we get walls when we're in the top left corner."
                       False
                       (isFullBoard p1IBoard))

fbP2 = TestCase (assertEqual
                       "Assert that we get walls when we're in the top left corner."
                       False
                       (isFullBoard p2IBoard))

fbFull = TestCase (assertEqual
                       "Assert that isFullBoard is true once we're past setup."
                       True
                       (isFullBoard provIBoard))

-- Proximity tests.
proxTests = TestList [TestLabel "Free Space Proximity" proxEmpt,
                      TestLabel "Wall Proximity" proxWall,
                      TestLabel "'Trapped by Walls' Proximity" proxTrap,
                      TestLabel "Ramp Proximity" proxRamp,
                      TestLabel "Player Proximity" proxPlayers]

proxEmpt = TestCase (assertEqual
                     "Assert that we can get a proximity from empty spaces."
                     [Space (IPt 0 0) 0, Space (IPt 0 1)  0, Space (IPt 0 2) 0,
                      Space (IPt 1 0) 0, Space (IPt 1 1)  0, Space (IPt 1 2) 0,
                      Space (IPt 2 0) 0, Space (IPt 2 1)  0, Space (IPt 2 2) 0
                     ]
                     (getProx emptIBoard (IPt 1 1)))

proxWall = TestList [TestLabel "Top Left Corner" proxWallTL,
                     TestLabel "Top Right Corner" proxWallTR,
                     TestLabel "Bottom Left Corner" proxWallBL,
                     TestLabel "Bottom Right Corner" proxWallBR]


proxWallTL = TestCase (assertEqual
                       "Assert that we get walls when we're in the top left corner."
                       [Wall , Wall              , Wall             ,
                        Wall , Player (IPt 0 0) 0, Space (IPt 0 1) 0,
                        Wall , Space  (IPt 1 0) 0, Space (IPt 1 1) 0
                       ]
                       (getProx cwiseIBoard (IPt 0 0)))

proxWallTR = TestCase (assertEqual
                       "Assert that we get walls when we're in the top right corner."
                       [Wall             , Wall              , Wall,
                        Space (IPt 0 3) 0, Player (IPt 0 4) 0, Wall,
                        Space (IPt 1 3) 0, Space  (IPt 1 4) 0, Wall
                       ]
                       (getProx cwiseIBoard (IPt 0 4)))

proxWallBL = TestCase (assertEqual
                       "Assert that we get walls when we're in the bottom left corner."
                       [Space (IPt 3 3) 0, Space  (IPt 3 4) 0, Wall,
                        Space (IPt 4 3) 0, Player (IPt 4 4) 0, Wall,
                        Wall             , Wall              , Wall
                       ]
                       (getProx cwiseIBoard (IPt 4 4)))

proxWallBR = TestCase (assertEqual
                       "Assert that we get walls when we're in the bottom right corner."
                       [Wall , Space  (IPt 3 0) 0, Space (IPt 3 1) 0,
                        Wall , Player (IPt 4 0) 0, Space (IPt 4 1) 0,
                        Wall , Wall              , Wall
                       ]
                       (getProx cwiseIBoard (IPt 4 0)))

proxTrap = TestCase (assertEqual
                     "Assert that we get walls when we're near capped towers."
                     [Wall , Wall              , Wall,
                      Wall , Player (IPt 1 1) 0, Wall,
                      Wall , Wall              , Wall
                     ]
                     (getProx trapIBoard (IPt 1 1)))

proxRamp = TestCase (assertEqual
                     "Assert that we get proper ordering and values by checking the Ramp board"
                     [Space (IPt 0 0) 2 ,Space  (IPt 0 1) 3, Space (IPt 0 2) 2,
                      Space (IPt 1 0) 1 ,Player (IPt 1 1) 0, Space (IPt 1 2) 2,
                      Space (IPt 2 0) 2 ,Space  (IPt 2 1) 3, Space (IPt 2 2) 2
                     ]
                     (getProx ramp2WinIBoard (IPt 1 1)))

proxPlayers = TestList [TestLabel "Player proximity on flat ground." proxPlayersFlat,
                        TestLabel "Player proximity on a set of steps" proxPlayersSteps]

proxPlayersFlat = TestCase (assertEqual
                        "Assert that we get players back from proximity (Flat)"
                        [
                         Space (IPt 2 2) 0 ,Space  (IPt 2 3) 0, Space  (IPt 2 4) 0,
                         Space (IPt 3 2) 0 ,Player (IPt 3 3) 0, Player (IPt 3 4) 0,
                         Space (IPt 4 2) 0 ,Player (IPt 4 3) 0, Player (IPt 4 4) 0
                        ]
                        (getProx emptIBoard (IPt 3 3)))

proxPlayersSteps = TestCase (assertEqual
                        "Assert that we get players back from proximity (Steps)"
                        [
                         Player (IPt 0 0) 3, Player (IPt 0 1) 2, Space (IPt 0 2) 0,
                         Player (IPt 1 0) 2, Player (IPt 1 1) 1, Space (IPt 1 2) 0,
                         Space  (IPt 2 0) 0, Space  (IPt 2 1) 0, Space (IPt 2 2) 0
                        ]
                        (getProx p1WIBoard (IPt 1 1)))
