module AIKernels where

import SantoriniRep
import SantoriniLogic

-- Note: The identityKernel is now just another term for id. It didn't need its
--       own function alias.

-- A pure MetaKernel that forwards to subkernels based upon a list of predicates.
-- When passed to a list of (predicate, kernel) pairs, it becomes an AIKernel
-- that calls different kernels, according to its predicates. The predicates are
-- evaluated in order, and the first matching predicate is called. If none are
-- called, the identity kernel is called.
--   TODO: Warning about identity kernel fall through?
predKernel :: [(IBoard -> Bool, AIKernel)] -> IBoard -> IBoard
predKernel [] brd = brd
predKernel preds brd = kern brd
  where kern = case dropWhile (`fst` brd) preds of []     -> id
                                                   (x:xs) -> snd x

-- A pure setup kernel that tries to place pieces in all four corners of the
-- board, in a clockwise manner, starting from the top. Not a great strategy,
-- but great for validating startup kernel behavior externally.
cornerSetup :: IBoard -> IBoard
cornerSetup brd = newBrd
  where -- TODO: This is ugly. List comprehension, like getTok?
        tl = IPt 0             0
        tr = IPt 0             (col gBrdBnd)
        bl = IPt (row gBrdBnd) 0
        br = IPt (row gBrdBnd) (col gBrdBnd)
        startingPts = [tl, tr, bl, br]
        -- TODO: isSpace
        freeSpaces = dropWhile (not . isSpace . getTok brd) startingPts
        chosenSpaces = case freeSpaces of (s1:s2:ss) -> (s1, s2)
                                          _ -> error "Couldn't find a free space in cornerSetup"
        -- Place a player on the spaces.
        newBrd = foldl placePlayer brd chosenSpaces

-- A pure gametime kernel that just moves the first player to the first available
-- space, and then builds a tile on the space it moved from. When it loses a player,
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
