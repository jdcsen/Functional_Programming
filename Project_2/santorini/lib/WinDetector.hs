
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

class WinDetector a where
  isWon :: a -> IBoard -> Action -> WinState

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


-- TODO: Combine semigroup win states with base win detector
-- functionality in order to implement a Turn-level Win/Loss
-- detector. Turn Generators could generate all legal moves,
-- and trim losing/winning ones until they don't have any
-- extra moves.
