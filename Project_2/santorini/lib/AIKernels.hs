module AIKernels where
import Control.Exception
import SantoriniLogic
import SantoriniRep
import Turns
import TurnGenerators

import qualified Data.Set as S
import Data.Maybe
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
  where
    kern = case dropWhile (\a -> not (fst a brd)) preds of
      [] -> id
      (x : xs) -> snd x

-- A pure setup kernel that tries to place pieces in all four corners of the
-- board, in a clockwise manner, starting from the top. Not a great strategy,
-- but great for validating startup kernel behavior externally.
cornerSetup :: IBoard -> IBoard
cornerSetup brd = newBrd
  where
    -- TODO: This is ugly. List comprehension, like getTok?
    tl = IPt 0 0
    tr = IPt 0 (col gBrdBnd)
    bl = IPt (row gBrdBnd) 0
    br = IPt (row gBrdBnd) (col gBrdBnd)
    startingPts = [tl, tr, bl, br]
    -- TODO: isSpace
    freeSpaces = filter (isSpace . getTok brd) startingPts
    chosenSpaces = case freeSpaces of
      (s1 : s2 : ss) -> [s1, s2]
      _ -> error "Couldn't find a free space in cornerSetup"
    -- Place a player on the spaces.
    newBrd = foldl placePlayer brd chosenSpaces

-- TODO: Kernel agnostic error checking wrapper.

-- A pure gametime kernel that just moves the first player to the first available
-- space, and then builds a tile on the space it moved from. When it loses a player,
-- it just starts moving the other one.
scorchedEarth :: IBoard -> IBoard
scorchedEarth brd = new_brd
  where
    -- Select the first movable player.
    -- If we don't have one, you lost. Currently crashes.
    player = getOurPlayer brd
    pred = not . null . getMoveable brd :: IPt -> Bool
    source = case filter pred (itokens player) of
      (p1 : ps) -> p1 :: IPt
      _         -> throw $ UndefinedElement "Scorched Earth: No movable player."

    moveTok = case getMoveable brd source of
      (move : ms) -> move :: BrdTok
      _            -> throw $ UndefinedElement "Scorched Earth: No target move."

    target = getPos moveTok :: IPt

    -- Execute the move.
    moved_brd = movePlayer brd (source, target) :: IBoard

    -- Build on the source location, if we haven't won.
    new_brd = if isStaticWon moved_brd
              then moved_brd
              else buildLvl moved_brd source


-- A kernel that selects the highest value move from the card-specific turn
-- generator.
hmoveCard :: IBoard -> IBoard
hmoveCard brd = newBrd
  where
    -- Generate moves
    card = icard $ getOurPlayer brd
    moves = genMoves card brd
    move = fromMaybe (error "No available moves") $ S.lookupMax moves
    newBrd = mut brd move
