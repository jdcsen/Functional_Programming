{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SantoriniRep where

import Data.Aeson
import GHC.Generics

-- Datastructures to ease working with boards:

-- Distinguish our indexing on basis of type
data IPt = IPt {row :: Int,
                col :: Int} deriving (Eq, Show)

-- TODO: Should I make these arrays fixed length? I don't
-- want to write a custom encoder/decoder.
newtype JPt = JPt [Int] deriving (Eq, Show, Generic)

-- If we deserialize an empty list, we get an empty JPt.
-- This lets us filter them out.
jPtValid :: JPt -> Bool
jPtValid (JPt [row, col]) = True
jPtValid _ = False

instance FromJSON JPt
instance ToJSON   JPt

-- Our board representation. Pretty simple, has a
-- turn count, arrays for spaces and players.
--
-- We have two forms of this: JBoard, which is perfectly
-- aligned to the output requirements, and IBoard, which
-- we're more free to manipulate. We provide toJBoard and
-- fromJBoard functions..
data JBoard = JBoard { turn    :: Maybe Int,
                       spaces  :: Maybe [[Int]],
                       players :: [[JPt]]
                     } deriving (Show, Generic, Eq)

instance FromJSON JBoard
instance ToJSON   JBoard

-- All internal functions use IBoard for consistency.
-- Right now, all we do is make the player points
-- zero-indexed so we can use them to index into the board.
data IBoard = IBoard { iturn :: Maybe Int,
                       ispaces :: Maybe [[Int]],
                       iplayers :: [[IPt]]
                     } deriving (Show, Generic, Eq)


fromJBoard :: JBoard -> IBoard
fromJBoard JBoard{turn   = trn,
                  spaces  = spc,
                  players = plrs}  = iBoard
  where vpts = map (filter jPtValid) plrs
        iBoard = IBoard { iturn    = trn,
                          ispaces  = spc,
                          iplayers = map (map jPt2iPt) vpts
                        }

toJBoard   :: IBoard -> JBoard
toJBoard IBoard{iturn    = trn,
                ispaces  = spc,
                iplayers = plrs} = jBoard
  where jBoard = JBoard { turn    = trn,
                          spaces  = spc,
                          players = map (map iPt2jPt) plrs
                        }

-- Our AI's are defined in terms of AI Kernels, which just
-- map IBoards to new IBoards.
type AIKernel = IBoard -> IBoard

-- Conversion functions.
iPt2jPt :: IPt -> JPt
iPt2jPt bpt = JPt [row bpt, col bpt]

jPt2iPt :: JPt -> IPt
jPt2iPt (JPt (row:col:xs)) = IPt (row-1) (col-1)

-- Tokens to build neighborhoods from.
-- Turn decisions are often determined in terms of a
-- neighborhood, rather than an absolute position. We
-- have to preserve location information, so they can
-- be mapped back to the board (except for walls, which
-- are immutable)
data BrdTok where
  -- A given space, parameterized by location and height.
  Space  :: IPt -> Int -> BrdTok
  -- A player token, parameterized by location.
  Player :: IPt -> BrdTok
  -- A wall. Impassable, not buildable.
  Wall   :: BrdTok
  deriving (Eq, Show)
