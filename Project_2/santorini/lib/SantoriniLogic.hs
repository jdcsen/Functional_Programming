{-# LANGUAGE LambdaCase #-}

module SantoriniLogic where
import SantoriniRep
import Data.Maybe
import Data.List
import Data.Data

-- Determines whether or not the board is in a play state.
isFullBoard :: IBoard -> Bool
isFullBoard IBoard{iturn   = -1,
                   ispaces  = _,
                   iplayers = playArr} = False
isFullBoard _ = True

-- Given a board and a point, returns the Board Token at that point. Does not
-- have to respect the boundaries of the board Array, anything out of range is a Wall
getTok :: IBoard -> IPt -> BrdTok
getTok brd pt
  | out_of_range = Wall
  | height >= gMaxTower = Wall
  | has_player   = Player pt height
  | otherwise    = Space pt height
  where -- I thought this was clever. Rows and columns need
        -- to be checked against each other, so I parameterized
        -- by the functions themselves.
        out_of_range = any (\a -> a pt > a gBrdBnd || a pt < 0)
                           [row, col]
        ptelem = elem pt :: [IPt] -> Bool
        has_player = any ptelem $ iplayers brd
        height = if not out_of_range then (ispaces brd !! row pt) !! col pt
                                     else -1

-- Given a position, returns the proximity of that position.
-- Move decisions are made in terms of proximity. Cells are ordered clockwise
-- starting from the top left.
getProx :: IBoard-> IPt -> [BrdTok]
getProx brd pt = vals
  where osets = [1,0,-1]
        points = [IPt (row pt - row_oset)
                      (col pt - col_oset) | row_oset <- osets,
                                            col_oset <- osets]
        vals = map (getTok brd) points

-- Given a board and a position, returns all the buildable tiles near that position.
getBuildable :: IBoard -> IPt -> [BrdTok]
getBuildable brd pt = buildable
  where predicate = \case (Space _ _) -> True
                          _           -> False
        filt = filter predicate
        buildable = filt $ getProx brd pt

-- Given a board and a position, returns all the movable tiles near that position.
getMoveable :: IBoard -> IPt -> [BrdTok]
getMoveable = getProx -- TODO: Implement.
