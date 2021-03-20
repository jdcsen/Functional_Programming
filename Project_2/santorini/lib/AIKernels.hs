{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module AIKernels where
import Control.Exception
import SantoriniLogic
import SantoriniRep
import Turns
import TurnGenerators
import WinDetector

import Data.List
import qualified Data.Set as S
import Data.Maybe
import System.Random
-- Note: The identityKernel is now just another term for id. It didn't need its
--       own function alias.


-- I specifically avoided making Kernels monadic, as I didn't think that it was
-- the right choice. It seemed like an over-abstraction, and I'm pretty hesitant
-- to write code using something I don't understand, after prior incidents in
-- this course.
--
-- In order to impose Data constructor constraints for Kernels, we group
-- all kernels together into a GADT. Mostly useful for predKernel.
data Kernel where
  -- A null kernel. Identity over the board. Intended as an error state.
  NullKernel :: Kernel

  -- ####### Kernel State Invariant Kernels: ###############

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

  -- ####### Kernel State Dependent Kernels: ###############

  -- Predicate kernels a list of Predicate-Kernel pairs.
  -- Predicate kernels dynamically choose and tick one of their internal kernels
  -- based on the passed board, updating the kernel, and marking it as last-used.
  -- Throws an error if we fall through.
  PredKernel     :: [PredPair] -> Kernel

  -- A random Kernel. Built with a random number generator, chooses an arbitrary move.
  RandKernel :: StdGen -> Kernel

  -- A Monte Carlo Tree Search with a depth of 1. Contains a generator to drive
  -- the Monte Carlo method, and an integer to specify how many samples to take.
  MCTS1 :: StdGen -> Int -> Kernel

  deriving (Show)

-- Define Kernel equality shallowly: If two top-level kernel types are identical,
-- the kernels are identical. Won't recursively check Predicate kernels.
--
-- Note: I _think_ I could do this with Generic Haskell, but I don't know much
-- about it, and I'm not opening up that can of worms right now. We'll get a
-- pattern match error if we're missing a case.
instance Eq Kernel where
  NullKernel == NullKernel  = True
  CornerSetup == CornerSetup       = True
  ScorchedEarth == ScorchedEarth   = True
  HValueDefault == HValueDefault   = True
  (PredKernel _)== (PredKernel _)  = True
  (RandKernel _)== (RandKernel _)  = True
  -- If not explicitly defined as True, False.
  _ == _                           = False

-- All Kernels define a tick method that applies their Kernel-specific logic
-- to the board..
tick :: (Kernel, IBoard) -> (Kernel, IBoard, Turn)

-- Identity kernels.
tick (NullKernel, brd) = (NullKernel, brd, Turn [])

-- Static Kernels.
tick (CornerSetup,   brd) = appKern CornerSetup   $ cornerSetup brd
tick (ScorchedEarth, brd) = appKern ScorchedEarth $ scorchedEarth brd
tick (HValueDefault, brd) = appKern HValueDefault $ hmoveCard brd

-- The predicate kernel must match out the kernel, tick it, and replace it.
tick (PredKernel preds, brd) = newPair
  where
    PP(pred, kern) = matchPair preds brd :: PredPair
    pair = PP(pred, kern)
    -- Tick the kernel.
    (newKern, newBrd, turn) = tick (kern, brd)
    -- Replace the kernel.
    rmKernPreds = delete pair preds :: [PredPair]
    newPreds = PP (pred, newKern) : rmKernPreds
    newPair = (PredKernel newPreds, newBrd, turn)

-- The random kernel extracts its generator, uses it to pick a move, then passes
-- it along to the next iteration.
tick (RandKernel gen, brd) = newPair
  where
    -- Generate moves
    card = icard $ getOurPlayer brd
    moves = genMoves card brd
    -- Select a random move index.
    (moveIdx, newGen) = uniformR (0, S.size moves-1) gen
    move = S.elemAt moveIdx moves
    -- Apply the move to the board.
    newBrd = mut brd move
    newPair = (RandKernel newGen, newBrd, move)

-- The Monte Carlo Tree Search (Depth 1) Kernel
tick (MCTS1 gen numSamples, brd) = newPair
  where
    -- Generate moves
    card = icard $ getOurPlayer brd
    moves = genMoves card brd
    -- Select a random move index.
    (moveIdx, newGen) = uniformR (0, S.size moves-1) gen
    move = S.elemAt moveIdx moves
    -- Apply the move to the board.
    newBrd = mut brd move
    newPair = (RandKernel newGen, newBrd, move)

-- Predicate Kernel pair
newtype PredPair = PP (IBoard -> Bool, Kernel)

instance Show PredPair where
  show p = "Predicate."

instance Eq PredPair where
  (==) (PP (_, a)) (PP (_, b)) = a == b


matchPair :: [PredPair] -> IBoard -> PredPair
matchPair [] _ = error "Predicate Kernel has no PredPairs"
matchPair preds brd = pair
  where
    pair = case dropWhile (\case PP (pred, kern) -> not (pred brd)) preds of
      [] -> error "Predicate kernel doesn't match"
      (x : xs) -> x :: PredPair

appKern :: Kernel -> (IBoard, Turn) -> (Kernel, IBoard,Turn)
appKern k (i, t) = (k, i, t)

-- TODO: Kernel agnostic error checking wrapper.
cornerSetup :: IBoard -> (IBoard, Turn)
cornerSetup brd = (newBrd, trn)
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
      (s1 : s2 : ss) -> [s1, s2] :: [IPt]
      _ -> error "Couldn't find a free space in cornerSetup"
    -- Place a player on the spaces.
    newBrd = foldl placePlayer brd chosenSpaces
    -- Build our turn description.
    trn = Turn $ map Place chosenSpaces


scorchedEarth :: IBoard -> (IBoard, Turn)
scorchedEarth brd = (newBrd, trn)
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

    -- Choose our move.
    move = Move source $ getPos moveTok :: Action
    -- Build on the source location
    build = Build source
    trn = Turn [move, build]
    -- Trim the turn.
    bseTrim = trimBaseTurn brd trn
    -- Trim the term, with the card.
    crdTrim = trimTurn (icard player) brd bseTrim
    -- Execute the move.
    newBrd = mut brd crdTrim

-- A kernel that selects the highest value move from the card-specific turn
-- generator.
hmoveCard :: IBoard -> (IBoard, Turn)
hmoveCard brd = (newBrd, move)
  where
    -- Generate moves
    card   = icard $ getOurPlayer brd
    moves  = genMoves card brd
    move   = fromMaybe (error "No available moves") $ S.lookupMax moves
    newBrd = mut brd move

-- Plays out two kernels from a base state until one of them wins.
playout :: IBoard -> (Kernel, Kernel)  -> (Kernel, IBoard, Turn)
playout brd (p1, p2)
  | not movable            = (p1, newBrd, trn)
  | wonLost /= Neither     = (p1, newBrd, trn)
  | otherwise              = playout newBrd (p2, newP1)
  where
    -- Note: Our win checker expects us to not even get a board if we don't have
    --       valid moves, as the drivers do it for us. Because of this, we have
    --       to check for blocked states in our playout, separate from other win
    --       states.
    movable = head $ hasMoves brd
    (newP1, trnBrd, trn) = if movable
                              then tick (p1, brd)
                              else (p1, brd, Turn [])
    -- Increment turn (if we moved).
    newBrd = if movable
                then incrementTurn trnBrd
                else trnBrd
    -- Check for a winning turn.
    wonLost = isWinningTurn newBrd trn

-- Given a board, take a single Monte Carlo sample from an initial board state,
-- and return whether or not we won.
sample :: StdGen -> IBoard -> (Bool, StdGen)
sample gen brd = (isWon, newGen)
  where
    ourP = getOurPlayer brd
    kern = RandKernel gen
    (outKern, outBoard, outTurn) = playout brd (kern, kern)
    -- Extract the random number generator from the out kernel.
    !newGen = case outKern of
               (RandKernel postRunGen) -> postRunGen
    -- If we're the first player
    isFp = getOurPlayer outBoard == ourP
    -- The turn is empty
    emptTurn = case outTurn of
                (Turn []) -> True
                _         -> False
    -- Our player won if it's the final player, AND it didn't get blocked.
    !isWon = isFp && not emptTurn
