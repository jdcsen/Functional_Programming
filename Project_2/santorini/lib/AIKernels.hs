module AIKernels where

import SantoriniRep
import SantoriniLogic

-- A pure identity kernel that does not execute any actions.
identityKernel :: IBoard -> IBoard
identityKernel = id

-- A pure kernel that just moves the first player to the first available space,
-- and then builds a tile on the space it moved from. When it loses a player,
-- it just starts moving the other one.
scorchedEarth :: IBoard -> IBoard
scorchedEarth brd = new_brd
  where -- Find movable players.
        players  = head $ iplayers brd
        pred     = not . null . getMoveable brd
        movableP = filter pred players

        -- Select the first movable player.
        -- If we don't have one, you lost. Currently crashes.
        source  = case movableP of [p1,p2] -> p1
                                   [p1]    -> p1
        move    = head $ getMoveable brd source
        target  = case move of (Space pt h) -> pt

        -- Execute the move.
        moved_brd = movePlayer brd source target

        -- Build on the source location.
        new_brd = buildLvl moved_brd source
