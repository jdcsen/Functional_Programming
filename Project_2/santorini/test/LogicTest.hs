module LogicTest where
import Test.HUnit
import SantoriniRep
import TestBoards
import SantoriniLogic

-- Top level test case.
logicTests = TestList [TestLabel "Proximity Function Tests" proxTests]

-- Proximity tests.
proxTests = TestList [TestLabel "Free Space Proximity" proxEmpt]


proxEmpt = TestCase (assertEqual
                     "Assert that we can get a proximity from empty spaces."
                     [Space (IPt 0 0) 0, Space (IPt 1 0) 0, Space (IPt 2 0) 0,
                      Space (IPt 0 1) 0, Space (IPt 1 1) 0, Space (IPt 2 1) 0,
                      Space (IPt 0 2) 0, Space (IPt 1 2) 0, Space (IPt 2 2) 0
                     ]
                     (getProx emptIBoard (IPt 2 2)))

