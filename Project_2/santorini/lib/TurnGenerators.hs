
module TurnGenerators where

import SantoriniRep
import SantoriniLogic
import Turns

class TGen a where
  genMoves :: a -> IBoard -> [Turn]

-- The base turn generator.
-- Generates all legal standard turns from a given board state
-- Can be used on its own, also appended to all card-specific
-- generated turns.
baseGen :: IBoard -> [Turn]
baseGen brd = []

-- Make our Card Enum an instance of TGen, so we can generate
-- turns from it.
instance TGen CardE where
  genMoves Apollo     brd = baseGen brd
  genMoves Artemis    brd = baseGen brd
  genMoves Atlas      brd = baseGen brd
  genMoves Demeter    brd = baseGen brd
  genMoves Hephastus  brd = baseGen brd
  genMoves Minotaur   brd = baseGen brd
  genMoves Pan        brd = baseGen brd
  genMoves Prometheus brd = baseGen brd
