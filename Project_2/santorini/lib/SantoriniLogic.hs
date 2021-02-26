{-# LANGUAGE LambdaCase #-}

module SantoriniLogic where
import SantoriniRep
import Data.Maybe
import Data.List
import Data.Data
import Control.Exception

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
  | height >= gIWallHeight = Wall
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
                                     else gIWallHeight

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
getMoveable brd pt = moveable
  where hght = getHeight $ getTok brd pt
        predicate = \case (Space _ shght) -> abs(hght - shght) <= gMaxTravel
                          _               -> False
        filt = filter predicate
        moveable = filt $ getProx brd pt

-- Given a token, returns the height of that token.
-- Walls have a height of 5, contradicting the JBoard representation. This allows
-- us to easily fit walls into our getMoveable code, because a board height of
-- 5 can be moved neither to nor from, given there's never a height of 4.
getHeight :: BrdTok -> Int
getHeight (Space pt height)  = height
getHeight (Player pt height) = height
getHeight Wall = gIWallHeight

-- Moves a player from a source location to a target location.
-- If no player is at the source location, throws UndefinedElement
movePlayer :: IBoard -> IPt -> IPt -> IBoard
movePlayer brd src tgt = newBrd
  where players = head $ iplayers brd
        -- Ensure we have a player to move.
        src = if src `elem` players
              then src
              else throw $ UndefinedElement "The source player does not exist."
        -- Replace the players.
        newPlayers =  (tgt : delete src players) : delete players (iplayers brd)
        -- Rebuild the board.
        --   Note: functions for single element replacement? Does record syntax
        --         give us anything like that?
        newBrd = IBoard {
                          iturn    = iturn brd,
                          ispaces  = ispaces brd,
                          iplayers = newPlayers
                        }

-- Builds a level at the target location.
-- If the element at the location is not buildable, throws UndefinedElement
buildLvl :: IBoard -> IPt -> IBoard
buildLvl brd loc = newBrd
  where -- Ensure we can build at this location.
        ht = case getTok brd loc
                 of (Space loc ht) -> ht
                    _              -> throw $ UndefinedElement "Tried to build on an invalid space."
        -- NOTE: We check for walls and players above, so this is safe.
        nHt = if ht == gMaxTower
              then gIWallHeight
              else ht + 1

        -- Update the spaces array.
        -- NOTE: This is messy. Clean it up.
        newRow    = replaceNth (col loc) nHt $ ispaces brd !! row loc
        newSpaces = replaceNth (row loc) newRow $ ispaces brd
        -- Rebuild the board.
        newBrd = IBoard {
                          iturn    = iturn brd,
                          ispaces  = newSpaces,
                          iplayers = iplayers brd
                        }

-- Places a player at the target location.
-- If the element at the location cannot have a player placed, throws UndefinedElement
placePlayer :: IBoard -> IPt -> IBoard
placePlayer brd loc = newBrd
  where -- Ensure we can place a player at this location.
        loc = case getTok brd loc
                 of (Space loc ht) -> loc
                    _              -> throw $ UndefinedElement "Tried to place a player on an invalid space."

        -- Build the new player array.
        --   Note: Currently, strictly matches arrays of size 1 and 2, so we fail
        --         on other cases. There might be a better way to do this.
        newP = case iplayers brd of [[]]                -> [[loc]]
                                    [[p11]]             -> [loc : [p11]]
                                    [[p21, p22]]        -> [[loc], [p21, p22]]
                                    [[p11], [p21, p22]] -> [loc : [p11], [p21, p22]]
        -- Rebuild the board.
        newBrd = IBoard {
                          iturn    = iturn brd,
                          ispaces  = ispaces brd,
                          iplayers = newP
                        }

