
module WinDetector where

import SantoriniRep
import SantoriniLogic
import Turns

gPanDropWin = 2

data WinState =
  Win     |
  Loss    |
  Neither deriving (Enum, Show, Eq)

-- If associating Win/Loss states, defer to the earliest
-- Win/Lose result.
instance Semigroup WinState where
  (<>) Win Loss  = Win
  (<>) l Loss    = Loss
  (<>) Loss Win  = Loss
  (<>) l Win     = Win
  (<>) l Neither = l

invertWinState :: WinState -> WinState
invertWinState Win  = Loss
invertWinState Loss = Win
invertWinState x = x

class WinDetector dtr where
  isWon :: dtr -> IBoard -> Action -> WinState

-- The base win detector. For all cards, a Move which results
-- in the token reaching level 3 from a lower level is considered a win.
baseIsWon :: IBoard -> Action -> WinState
baseIsWon brd (Move a b) = winState
  where
    aHeight = getHeight $ getTok brd a
    bHeight = getHeight $ getTok brd b
    winState = if aHeight < gMaxTower && bHeight == gMaxTower
                  then Win
                  else Neither
baseIsWon _ _ = Neither

-- We handle additional win states by or'ing the base win
-- detector with card-specific win detectors.
instance WinDetector CardE where
  -- Pan and Minotaur have special cases.

  -- Pan can win by moving down 2+ spaces.
  isWon Pan brd action@(Move a b) = panWin <> baseIsWon brd action
    where
      aHeight = getHeight $ getTok brd a
      bHeight = getHeight $ getTok brd b
      panWin = if aHeight - bHeight >= gPanDropWin
                then Win
                else Neither

  -- The Minotaur can _lose_ by pushing Pan off a cliff.
  -- TODO: Make this a more general "if the other player wins because of
  --       something you did, you lose". Maybe use some form of "Displacement"
  --       action applied to the second game piece, like shown in class?
  isWon Minotaur brd action@(Push a b) = minoLoss <> baseIsWon brd action
    where
      pLoc = pushLoc (a, b)
      oPlayer = getOtherPlayer brd
      minoLoss = if Pan == icard oPlayer
                    then invertWinState $ isWon Pan brd (Move b pLoc)
                    else Neither

  -- Blanket cases for all other instances.
  isWon Apollo     brd action =
    baseIsWon brd action
  isWon Artemis    brd action =
    baseIsWon brd action
  isWon Atlas      brd action =
    baseIsWon brd action
  isWon Demeter    brd action =
    baseIsWon brd action
  isWon Hephastus  brd action =
    baseIsWon brd action
  isWon Minotaur   brd action =
    baseIsWon brd action
  isWon Pan        brd action =
    baseIsWon brd action
  isWon Prometheus brd action =
    baseIsWon brd action

-- Given a turn, trims the turn until we have either a single Win, a single Loss,
-- or all Neither. Version for operating with just the base win detector.
trimBaseTurn :: IBoard -> Turn -> Turn
trimBaseTurn brd (Turn actions) = trimmedTurn
  where
    sortedActions = span (\a -> baseIsWon brd a == Neither) actions
    trimmedTurn = case sortedActions of
                    (nMoves, []) -> Turn nMoves
                    (nMoves, wlMoves) -> Turn (nMoves ++ [head wlMoves])

-- Given a turn, trims the turn until we have either a single Win, a single Loss,
-- or all Neither
trimTurn :: (WinDetector a) => a -> IBoard -> Turn -> Turn
trimTurn wd brd (Turn actions) = trimmedTurn
  where
    sortedActions = span (\a -> isWon wd brd a == Neither) actions
    trimmedTurn = case sortedActions of
                    (nMoves, []) -> Turn nMoves
                    (nMoves, wlMoves) -> Turn (nMoves ++ [head wlMoves])

-- Given a Board and a list of actions, splits the list of actions into winning,
-- losing, and neither moves.
splitActions :: IBoard -> [Action] -> ([Action], [Action], [Action])
splitActions brd actions = (winning, losing, neither)
  where
    winning =
      filter
        (\a -> baseIsWon (mut brd a) a == Win)
        actions
    losing =
      filter
        (\a -> baseIsWon (mut brd a) a == Loss)
        actions
    neither =
      filter
        (\a -> baseIsWon (mut brd a) a == Neither)
        actions
