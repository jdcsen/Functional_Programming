{-# LANGUAGE LambdaCase #-}

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

-- Use move logic to build pushes.
buildPush  :: BrdTok -> BrdTok -> Maybe Action
buildPush a b =
  case buildMove a b of
    (Just (Move ploc sloc)) -> Just $ Push ploc sloc
    _ -> Nothing

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
    -- Build our pipeline to build move Actions from a Player loc.
    mPline =
      (\toc -> mapMaybe
                (buildMove toc)
                (getProx brd (getLoc toc))) :: BrdTok -> [Action]
    pToks = playerToBrdTok brd $ getOurPlayer brd

    -- Build our move actions, and sort out any winners.
    moveActions = concatMap mPline pToks :: [Action]
    (wAct, lAct, nAct) = splitActions brd moveActions

    -- Populate moves with winning actions to start.
    wMoves = map (\a -> Turn [a]) wAct :: [Turn]
    lMoves = map (\a -> Turn [a]) lAct :: [Turn]

    -- Zip a list of move actions and resulting boards, for use in build generation.
    moveBoards = zip nAct (map (mutate brd) nAct)

    -- Function to extract build moves from our boards (as well as build the Turn)
    bPline =
      \case
        (Move from_pos to_pos, brd) ->
          map
            (\a -> Turn [Move from_pos to_pos, a])
            (mapMaybe buildBuild $ getBuildable brd to_pos)
    bMoves = concatMap bPline moveBoards :: [Turn]

    -- Build the move set.
    baseMoves = S.fromList $ wMoves ++ bMoves ++ lMoves

-- Make our Card Enum an instance of TGen, so we can generate
-- turns from it.
instance TGen CardE where

  -- Apollo — A token’s move can optionally swap places with an adjacent opponent
  -- token, as long as the token would be able to move to the opponent’s space
  -- if the opponent token were not there; otherwise, the move must be to an
  -- unoccupied space as usual.
  genMoves Apollo     brd = baseGen brd `S.union` apolloMoves
    where moves = []
          apolloMoves = S.fromList moves

  -- Artemis — The moved token can optionally move a second time (i.e., the same
  -- token), as long as the first move doesn't win, and as long as the second
  -- move doesn't return to the original space.
  genMoves Artemis    brd = baseGen brd `S.union` artemisMoves
    where moves = []
          artemisMoves = S.fromList moves

  -- Atlas — The build phase can build a space currently at level 0, 1, 2 to
  -- make it level 4, instead of building to exactly one more than the space’s
  -- current level.
  genMoves Atlas      brd = baseGen brd `S.union` atlasMoves
    where moves = []
          atlasMoves = S.fromList moves

  -- Demeter — The moved token can optionally build a second time, but not on
  -- the same space as the first build within a turn.
  genMoves Demeter    brd = baseGen brd `S.union` demeterMoves
    where moves = []
          demeterMoves = S.fromList moves

  -- Hephaestus — The moved token can optionally build a second time, but only
  -- on the same space as the first build within a turn, and only if the second
  -- build does not reach level 4.
  genMoves Hephastus  brd = baseGen brd `S.union` hepastusMoves
    where moves = []
          hepastusMoves = S.fromList moves

  -- Minotaur — A token’s move can optionally enter the space of an opponent’s
  -- token, but only if the token can be pushed back to an unoccupied space, and
  -- only as long as the token would be able to move to the opponent’s space if
  -- the opponent token were not there. The unoccupied space where the opponent’s
  -- token is pushed can be at any level less than 4. Note that the opponent
  -- does not win by having a token forced to level 3; furthermore, such a token
  -- will have to move back down before it can move to level 3 for a win.
  genMoves Minotaur   brd = baseGen brd `S.union` minotaurMoves
    where moves = []
          minotaurMoves = S.fromList moves

  -- Pan — A token can win either by moving up to level 3 or by moving down two
  -- or more levels. (Moving down three levels is possible if a token was pushed
  -- by a Minotaur.)
  genMoves Pan        brd = baseGen brd `S.union` panMoves
    where moves = []
          panMoves = S.fromList moves

  -- Prometheus — A token can optionally build before moving, but then the move
  -- is constrained to the same level or lower (i.e., the level of the token’s
  -- new space can be no larger than the level of the token’s old space). The
  -- moved token must still build after moving.
  genMoves Prometheus brd = baseGen brd `S.union` prometheusMoves
    where moves = []
          prometheusMoves = S.fromList moves
