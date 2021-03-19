{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module AIKernels where
import Control.Exception
import SantoriniLogic
import SantoriniRep
import Turns
import TurnGenerators

import Data.List
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State.Strict
-- Note: The identityKernel is now just another term for id. It didn't need its
--       own function alias.


-- I specifically avoided making Kernels monadic, as I didn't think that it was
-- the right choice. It seemed like an over-abstraction.
--
-- In order to impose Data constructor constraints for Kernels, we group
-- all kernels together into a GADT. Mostly useful for predKernel.
data Kernel where
  -- A null kernel. No transformations defined, intended to error. Used for parse
  -- failures.
  NullKernel :: Kernel
  -- Predicate kernels a list of Predicate-Kernel pairs.
  -- Predicate kernels dynamically choose and tick one of their internal kernels
  -- based on the passed board, updating the kernel, and marking it as last-used.
  -- Throws an error if we fall through.
  PredKernel     :: [PredPair] -> Kernel

  -- ####### Kernel State Invariant Kernels: ###############
  --
  -- A pure setup kernel that tries to place pieces in all four corners of the
  -- board, in a clockwise manner, starting from the top. Not a great strategy,
  -- but great for validating startup kernel behavior externally.
  CornerSetup   :: Kernel

  -- A pure gametime kernel that just moves the first player to the first available
  -- space, and then builds a tile on the space it moved from. When it loses a player,
  -- it just starts moving the other one.
  ScorchedEarth :: Kernel

  -- A kernel that selects the highest value move from the card-specific turn
  -- generator. Uses default turn values.
  HValueDefault :: Kernel

-- Define Kernel equality shallowly: If two top-level kernel types are identical,
-- the kernels are identical. Won't recursively check Predicate kernels.
--
-- Note: I _think_ I could do this with Generic Haskell, but I don't know much
-- about it, and I'm not opening up that can of worms right now. We'll get a
-- pattern match error if we're missing a case.
instance Eq Kernel where
  (PredKernel _)== (PredKernel _)  = True
  CornerSetup == CornerSetup       = True
  ScorchedEarth == ScorchedEarth   = True
  HValueDefault == HValueDefault   = True
  -- If not explicitly defined as True, False.
  _ == _                           = False

-- All Kernels define a tick method that applies their Kernel-specific logic
-- to the board..
tick :: Kernel -> IBoard -> (Kernel, IBoard)
  -- Implementations for static kernels are fairly straightforward.
tick CornerSetup   brd = (CornerSetup, cornerSetup brd)
tick ScorchedEarth brd = (ScorchedEarth, scorchedEarth brd)
tick HValueDefault brd = (HValueDefault, hmoveCard brd)

-- The predicate kernel must match out the kernel, tick it, and replace it.
-- TODO: Implement
tick (PredKernel preds) brd = newPair
  where
    (pred, kern) = matchPair preds brd :: PredPair
    -- Tick the kernel.
    (newKern, newBrd) = tick kern brd
    -- Replace the kernel.
    rmKernPreds = deleteBy (\a b -> snd a == snd b) (pred, kern) preds :: [PredPair]
    newPreds = (pred, newKern) : rmKernPreds
    newPair = (PredKernel newPreds, newBrd)

-- Predicate Kernel pair
type PredPair = (IBoard -> Bool, Kernel)

matchPair :: [PredPair] -> IBoard -> PredPair
matchPair [] _ = error "Predicate Kernel has no PredPairs"
matchPair preds brd = pair
  where
    pair = case dropWhile (\case (pred, kern) -> not (pred brd)) preds of
      [] -> error "Predicate kernel doesn't match"
      (x : xs) -> x :: PredPair


-- TODO: Kernel agnostic error checking wrapper.
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

