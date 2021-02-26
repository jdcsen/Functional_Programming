module TestBoards where
import SantoriniRep
-- Some standard boards, for testing.

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
                     spaces = Just [[2,2,2,2,2],
                                    [2,0,2,0,2],
                                    [2,2,2,2,2],
                                    [2,0,2,0,2],
                                    [2,2,2,2,2]],
                     players = [[JPt [2,2], JPt [2,4]],
                                [JPt [4,2], JPt [4,4]]]
                    }
trapIBoard = fromJBoard trapJBoard

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
