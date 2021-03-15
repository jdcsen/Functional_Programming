{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module TurnGenerators where

import SantoriniRep
import SantoriniLogic
import Turns
import WinDetector

import qualified Data.Set as S
import Data.Maybe

-- A set of convenience functions to transform board tokens
-- to legal actions.
--
-- Note: These functions do not check for proximity, as we
--       should be using proximity functions to pull Board
--       Tokens from our IBoard
buildPlace :: BrdTok -> Maybe Action
buildPlace (Space loc _) = Just $ Place loc
buildPlace _ = Nothing

buildBuild :: BrdTok -> Maybe Action
buildBuild (Space loc _) = Just $ Build loc
buildBuild _ = Nothing

buildCap :: BrdTok -> Maybe Action
buildCap (Space loc _) = Just $ Cap loc
buildCap _ = Nothing

-- Note: This is _technically_ duplicate logic, but it allows
-- us to feed build* functions from proximity alone.
buildMove  :: BrdTok -> BrdTok -> Maybe Action
buildMove (Player ploc pht) (Space sloc sht)
  | abs(pht - sht) <= 1 = Just $ Move ploc sloc
  | otherwise           = Nothing

buildMove _ _ = Nothing

-- Use move logic to build swaps.
buildSwap  :: BrdTok -> BrdTok -> Maybe Action
buildSwap a b =
  case buildMove a b of
    (Just (Move ploc sloc)) -> Just $ Swap ploc sloc
    _ -> Nothing

buildPush  :: IBoard -> BrdTok -> BrdTok -> Maybe Action
buildPush brd (Player p1Loc p1Ht) (Player p2Loc p2Ht)
  | abs(p1Ht - p2Ht) <= 1 &&
    isSpace (getTok brd (pushLoc (p1Loc, p2Loc))) = Just $ Push p1Loc p2Loc
  | otherwise           = Nothing

buildPush _ _ _ = Nothing

-- Turn-ified versions of our base getBuildable, getMoveable, etc.

-- Generates all place options for a given board and IPlayer.
placeGen :: IBoard -> [(Action, (Agent, IBoard))]
placeGen brd = places
  where
    actions = mapMaybe buildPlace $ getAllTok brd
    agents  = map (\case (Place loc) -> loc) actions
    boards  = map (mut brd) actions
    places  = zip actions (zip agents boards)


-- Generates all build options for a given board and IPlayer.
buildGen :: (Agent, IBoard) -> [(Action, (Agent, IBoard))]
buildGen in_tup = builds
  where
    agnt = fst in_tup
    brd = snd in_tup
    actions = mapMaybe buildBuild (getProx brd agnt)
    tups = map (mutTup in_tup) actions
    builds  = zip actions tups

-- Generates all cap options for a given board and IPlayer.
capGen :: (Agent, IBoard) -> [(Action, (Agent, IBoard))]
capGen in_tup = builds
  where
    agnt = fst in_tup
    brd = snd in_tup
    actions = mapMaybe buildBuild (getProx brd agnt)
    tups = map (mutTup in_tup) actions
    builds  = zip actions tups

-- Generates all move options for a given board and IPlayer.
moveGen  :: (Agent, IBoard) -> [(Action, (Agent, IBoard))]
moveGen in_tup = moves
  where
    agnt = fst in_tup
    brd = snd in_tup
    actions =
      mapMaybe
        (buildMove $ getTok brd agnt)
        (getProx brd agnt)
    tups = map (mutTup in_tup) actions
    moves  = zip actions tups

-- Generates all Swap options for a given board and IPlayer
swapGen  :: (Agent, IBoard) -> [(Action, (Agent, IBoard))]
swapGen in_tup = swaps
  where
    agnt = fst in_tup
    brd = snd in_tup
    actions =
      mapMaybe
        (buildSwap $ getTok brd agnt)
        (getProx brd agnt)
    tups = map (mutTup in_tup) actions
    swaps  = zip actions tups

-- Generates all Push options for a given board and IPlayer
pushGen  :: (Agent, IBoard) -> [(Action, (Agent, IBoard))]
pushGen in_tup = pushes
  where
    agnt = fst in_tup
    brd = snd in_tup
    actions =
      mapMaybe
        (buildPush brd $ getTok brd agnt)
        (getProx brd agnt)
    tups = map (mutTup in_tup) actions
    pushes  = zip actions tups

-- Generates an action from an ActionE enum.
-- Not defined for PlaceE, as it does not accept an action as an input.
genAgentAction :: ActionE -> (Agent, IBoard) -> [(Action, (Agent, IBoard))]
genAgentAction PlaceE _   = error "genAgentAction is not defined for Place actions."
genAgentAction BuildE tup = buildGen tup
genAgentAction MoveE  tup = moveGen tup
genAgentAction SwapE  tup = swapGen tup
genAgentAction PushE  tup = pushGen tup
genAgentAction CapE   tup = capGen tup

-- Given a base turn, generates all actions and appends it to the turn.
genAgentTurn :: ActionE -> (Turn, (Agent, IBoard)) -> [(Turn, (Agent, IBoard))]
genAgentTurn actE tup = turnList
  where
    turn    = fst tup :: Turn
    agntBrd = snd tup :: (Agent, IBoard)
    turnAct = getActions turn :: [Action]
    -- Generate all actions from the base turn.
    actns = genAgentAction actE agntBrd
    -- Generate the new list of Turns, appending each action.
    appAct =
      \ actTup ->
        ( Turn (turnAct ++ [fst actTup]),
          snd actTup -- genAgentAction mutates for us.
        )
    turnList = map appAct actns

-- A version of genAgentAction that accepts a list of [(Turn, (Agent, IBoard)]
-- and expands the list with the provided action, duplicating each Turn and appending
-- the action. Supports optional actions.
genAgentTurn' :: [(Turn, (Agent, IBoard))] -> ActionE -> [(Turn, (Agent, IBoard))]
genAgentTurn' turnList action = concatMap (genAgentTurn action) turnList

-- Allows us to generate chains of actions, specifying certain actions as optional.
genAgentTurns :: [ActionE] -> (Agent, IBoard) -> [(Turn, (Agent, IBoard))]
genAgentTurns actions tup = []
  where
    -- Build our initial accumulator.
    emptAcc = [(Turn [], tup)]
    -- Left fold over our actions, building the accumulator with each step.
    turns = foldl genAgentTurn' emptAcc actions

type TurnSet = S.Set Turn

class TGen a where
  genMoves :: a -> IBoard -> TurnSet

-- The base turn generator.
-- Generates all legal standard turns from a given board state
-- Can be used on its own, also appended to all card-specific
-- generated turns.
--
-- Legal turns consist of two steps: A move, and a build executed by the moved
-- token, if the move did not result in a win.
baseGen :: IBoard -> TurnSet
baseGen brd = baseMoves
  where
    -- Generate all moves, for both Agents.
    actionTypes = [MoveE, BuildE]
    agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
    allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
    baseMoves = S.fromList $ map fst allTurns

-- Make our Card Enum an instance of TGen, so we can generate
-- turns from it.
instance TGen CardE where

  -- Apollo — A token’s move can optionally swap places with an adjacent opponent
  -- token, as long as the token would be able to move to the opponent’s space
  -- if the opponent token were not there; otherwise, the move must be to an
  -- unoccupied space as usual.
  genMoves Apollo     brd = baseGen brd `S.union` apolloMoves
    where
      -- Generate all moves, for both Agents.
      actionTypes = [SwapE, BuildE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- No filtering required.
      apolloMoves = S.fromList $ map fst allTurns

  -- Artemis — The moved token can optionally move a second time (i.e., the same
  -- token), as long as the first move doesn't win, and as long as the second
  -- move doesn't return to the original space.
  genMoves Artemis    brd = baseGen brd `S.union` artemisMoves
    where
      actionTypes = [MoveE, MoveE, BuildE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- TODO: Filter
      artemisMoves = S.fromList $ map fst allTurns

  -- Atlas — The build phase can build a space currently at level 0, 1, 2 to
  -- make it level 4, instead of building to exactly one more than the space’s
  -- current level.
  genMoves Atlas      brd = baseGen brd `S.union` atlasMoves
    where
      actionTypes = [MoveE, CapE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- Note: No filtering required.
      atlasMoves = S.fromList $ map fst allTurns

  -- Demeter — The moved token can optionally build a second time, but not on
  -- the same space as the first build within a turn.
  genMoves Demeter    brd = baseGen brd `S.union` demeterMoves
    where
      actionTypes = [MoveE, BuildE, BuildE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- TODO: Filter
      demeterMoves = S.fromList $ map fst allTurns

  -- Hephaestus — The moved token can optionally build a second time, but only
  -- on the same space as the first build within a turn, and only if the second
  -- build does not reach level 4.
  genMoves Hephastus  brd = baseGen brd `S.union` hepastusMoves
    where
      actionTypes = [MoveE, BuildE, BuildE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- TODO: Filter
      hepastusMoves = S.fromList $ map fst allTurns

  -- Minotaur — A token’s move can optionally enter the space of an opponent’s
  -- token, but only if the token can be pushed back to an unoccupied space, and
  -- only as long as the token would be able to move to the opponent’s space if
  -- the opponent token were not there. The unoccupied space where the opponent’s
  -- token is pushed can be at any level less than 4. Note that the opponent
  -- does not win by having a token forced to level 3; furthermore, such a token
  -- will have to move back down before it can move to level 3 for a win.
  genMoves Minotaur   brd = baseGen brd `S.union` minotaurMoves
    where
      actionTypes = [PushE, BuildE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- Note: No filtering required. Individual move validity handled elsewhere.
      minotaurMoves = S.fromList $ map fst allTurns

  -- Pan — A token can win either by moving up to level 3 or by moving down two
  -- or more levels. (Moving down three levels is possible if a token was pushed
  -- by a Minotaur.)
  --
  -- NOTE: Move capabilities are identical to base.
  genMoves Pan        brd = baseGen brd

  -- Prometheus — A token can optionally build before moving, but then the move
  -- is constrained to the same level or lower (i.e., the level of the token’s
  -- new space can be no larger than the level of the token’s old space). The
  -- moved token must still build after moving.
  genMoves Prometheus brd = baseGen brd `S.union` prometheusMoves
    where
      actionTypes = [BuildE, MoveE, BuildE]
      agents = zip (itokens $ getOurPlayer brd) (repeat brd) :: [(Agent, IBoard)]
      allTurns = concatMap (genAgentTurns actionTypes) agents :: [(Turn, (Agent, IBoard))]
      -- TODO: Filter
      prometheusMoves = S.fromList $ map fst allTurns
