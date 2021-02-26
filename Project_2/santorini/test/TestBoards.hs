module TestBoards where
import SantoriniRep
-- Some standard boards, for testing.

-- Player 1 Setup
p1BoardStr = "{\"players\":[[[]]]}"
p1JBoard = JBoard {turn= Nothing,
                   spaces = Nothing,
                   players = [[JPt []]]
                   }
p1IBoard = fromJBoard p1JBoard

-- Player 2 Setup
p2BoardStr = "{\"players\":[[[1,1],[2,2]]]}"
p2JBoard = JBoard {turn= Nothing,
                   spaces = Nothing,
                   players = [[JPt [1,1], JPt [2,2]]]
                   }
p2IBoard = fromJBoard p1JBoard

-- One of the provided boards.
provBoardStr = "{\"players\":[[[2,5],[3,5]],[[3,4],[4,4]]], \"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], \"turn\":19}"
provJBoard = JBoard {turn= Just 19,
                     spaces = Just [[0,0,0,0,2],
                                    [1,1,2,0,0],
                                    [1,0,0,3,0],
                                    [0,0,3,0,0],
                                    [0,0,0,1,4]],
                     players = [[JPt [2,5],JPt [3,5]],
                                [JPt [3,4],JPt [4,4]]]
                              }
provIBoard = fromJBoard provJBoard

-- An empty board, with the four players clustered in the
-- bottom left-hand corner.
emptBoardStr = "{\"players\":[[[4,4],[4,5]],[[5,4],[5,5]]], \"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]], \"turn\":1}"
emptJBoard = JBoard {turn   = Just 1,
                     spaces = Just [[0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0]],
                     players = [[JPt [4,4], JPt [4,5]],
                                [JPt [5,4], JPt [5,5]]]
                   }
emptIBoard = fromJBoard emptJBoard

-- An empty board, where the player positions are all in the corners, listed
-- alternatingly by player and clockwise.
cwiseBoardStr = "{\"players\":[[[1,1],[5,5]],[[1,5],[5,1]]], \"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]], \"turn\":1}"
cwiseJBoard = JBoard {turn   = Just 1,
                     spaces = Just [[0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0]],
                     players = [[JPt [1,1], JPt [5,5]],
                                [JPt [1,5], JPt [5,1]]]
                   }
cwiseIBoard = fromJBoard cwiseJBoard

-- An empty board, where the player positions are all in the corners, listed
-- alternatingly by player and counter-clockwise.
ccwiseBoardStr = "{\"players\":[[[5,1],[1,5]],[[5,5],[1,1]]], \"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]], \"turn\":1}"
ccwiseJBoard = JBoard {turn   = Just 1,
                     spaces = Just [[0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0],
                                    [0,0,0,0,0]],
                     players = [[JPt [5,1], JPt [1,5]],
                                [JPt [5,5], JPt [1,1]]]
                   }
ccwiseIBoard = fromJBoard ccwiseJBoard

-- A board where all players are trapped.
trapJBoard = JBoard {turn   = Just 1,
                     spaces = Just [[4,4,4,4,4],
                                    [4,0,4,0,4],
                                    [4,4,4,4,4],
                                    [4,0,4,0,4],
                                    [4,4,4,4,4]],
                     players = [[JPt [2,2], JPt [2,4]],
                                [JPt [4,2], JPt [4,4]]]
                    }
trapIBoard = fromJBoard trapJBoard

-- A board where all players are trapped. Buildable, just not movable.
btrapJBoard = JBoard {turn   = Just 1,
                      spaces = Just [[2,2,2,2,2],
                                     [2,0,2,0,2],
                                     [2,2,2,2,2],
                                     [2,0,2,0,2],
                                     [2,2,2,2,2]],
                      players = [[JPt [2,2], JPt [2,4]],
                                 [JPt [4,2], JPt [4,4]]]
                     }
btrapIBoard = fromJBoard trapJBoard

-- A board where players' only options is to go up a ramp to win.
ramp2WinJBoard = JBoard {turn   = Just 1,
                         spaces = Just [[2,3,2,3,2],
                                        [1,0,2,0,1],
                                        [2,3,2,3,2],
                                        [1,0,2,0,1],
                                        [2,2,2,2,2]],
                         players = [[JPt [2,2], JPt [2,4]],
                                    [JPt [4,2], JPt [4,4]]]
                        }
ramp2WinIBoard = fromJBoard ramp2WinJBoard

-- A board where players can't win without building, with ramps.
ramp2BuildJBoard = JBoard {turn   = Just 1,
                           spaces = Just [[2,2,2,2,2],
                                          [1,0,2,0,1],
                                          [2,2,2,2,2],
                                          [1,0,2,0,1],
                                          [2,2,2,2,2]],
                           players = [[JPt [2,2], JPt [2,4]],
                                      [JPt [4,2], JPt [4,4]]]
                          }
ramp2BuildIBoard = fromJBoard ramp2BuildJBoard

-- A board where all four players are clustered at the top right corner, with
-- the first player winning
p1WJBoard = JBoard {turn   = Just 1,
                    spaces = Just [[3,2,0,0,0],
                                   [2,1,0,0,0],
                                   [0,0,0,0,0],
                                   [0,0,0,0,0],
                                   [0,0,0,0,0]],
                    players = [[JPt [1,1], JPt [2,2]],
                               [JPt [2,1], JPt [1,2]]]
                   }
p1WIBoard = fromJBoard p1WJBoard

-- A board where all four players are clustered at the top right corner, with
-- the second player winning
p2WJBoard = JBoard {turn   = Just 1,
                    spaces = Just [[3,2,0,0,0],
                                   [2,1,0,0,0],
                                   [0,0,0,0,0],
                                   [0,0,0,0,0],
                                   [0,0,0,0,0]],
                    players = [[JPt [2,1], JPt [1,2]],
                               [JPt [1,1], JPt [2,2]]]
                   }
p2WIBoard = fromJBoard p1WJBoard
