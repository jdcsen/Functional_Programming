
module TurnGenerators where

import SantoriniRep
import SantoriniLogic
import Turns
import qualified Data.Set as S


type TurnSet = S.Set Turn

class TGen a where
  genMoves :: a -> IBoard -> TurnSet

-- The base turn generator.
-- Generates all legal standard turns from a given board state
-- Can be used on its own, also appended to all card-specific
-- generated turns.
baseGen :: IBoard -> TurnSet
baseGen brd = S.empty

-- Make our Card Enum an instance of TGen, so we can generate
-- turns from it.
instance TGen CardE where
  genMoves Apollo     brd = baseGen brd `S.union` apolloMoves
    where apolloMoves = S.empty
  genMoves Artemis    brd = baseGen brd
    where artemisMoves = S.empty
  genMoves Atlas      brd = baseGen brd
    where atlasMoves = S.empty
  genMoves Demeter    brd = baseGen brd
    where demeterMoves = S.empty
  genMoves Hephastus  brd = baseGen brd
    where hepastusMoves = S.empty
  genMoves Minotaur   brd = baseGen brd
    where minotaurMoves = S.empty
  genMoves Pan        brd = baseGen brd
    where panMoves = S.empty
  genMoves Prometheus brd = baseGen brd
    where prometheusMoves = S.empty
