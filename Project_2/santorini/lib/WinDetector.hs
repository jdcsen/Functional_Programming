
module WinDetector where

import SantoriniRep
import Turns

data WinState =
  Win     |
  Loss    |
  Neither deriving (Enum, Show, Eq)

-- If associating Win/Loss states, defer to the earliest
-- Win/Lose result.
instance Semigroup WinState where
  (<>) Win     l = Win
  (<>) Loss    l = Loss
  (<>) Neither l = l

class WinDetector dtr where
  isWon :: dtr -> IBoard -> Action -> WinState

-- The base win detector. For all cards, a Move which results
-- in the token reaching level 3 from a lower level is considered a win.
baseIsWon :: IBoard -> Action -> WinState
baseIsWon _ _ = Neither

-- We handle additional win states by or'ing the base win
-- detector with card-specific win detectors.
instance WinDetector CardE where
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
    baseIsWon brd action <> Neither
  isWon Prometheus brd action =
    baseIsWon brd action

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
