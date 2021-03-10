
module TurnGenerators where

import SantoriniRep
import SantoriniLogic
import Turns

class TGen a where
  genMoves :: IBoard -> a -> [Turn]

-- The base turn generator.
-- Generates all legal standard turns from a given board state
-- Can be used on its own, also appended to all card-specific
-- generated turns.
baseGen :: IBoard -> [Turn]
baseGen brd = []

-- Make our Card Enum an instance of TGen, so we can generate
-- turns from it.
instance TGen CardE where
  genMoves brd Apollo     = baseGen brd
  genMoves brd Artemis    = baseGen brd
  genMoves brd Atlas      = baseGen brd
  genMoves brd Demeter    = baseGen brd
  genMoves brd Hephastus  = baseGen brd
  genMoves brd Minotaur   = baseGen brd
  genMoves brd Pan        = baseGen brd
  genMoves brd Prometheus = baseGen brd
