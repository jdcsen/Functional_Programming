{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SantoriniRep where

import Data.Aeson
import Data.Maybe
import GHC.Generics

-- Global board information..
gBrdBnd      = IPt 4 4 :: IPt
gJWallHeight = 4       :: Int
gIWallHeight = 5       :: Int
gMaxTravel   = 1       :: Int

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
data IBoard = IBoard { iturn :: Int,
                       ispaces :: [[Int]],
                       iplayers :: [[IPt]]
                     } deriving (Show, Generic, Eq)


-- Converts from a JSON Board rep to our Internal Board rep
fromJBoard :: JBoard -> IBoard
fromJBoard JBoard{turn   = trn,
                  spaces  = spc,
                  players = plrs}  = iBoard
  where vpts = map (filter jPtValid) plrs
        rebuildWalls = map (map (\a -> if a == gJWallHeight then gIWallHeight else a))
        iBoard = IBoard { iturn    = fromMaybe (-1::Int) trn,
                          ispaces  = rebuildWalls $ fromMaybe (replicate (row gBrdBnd) $
                                                                replicate (col gBrdBnd) 0
                                                              ) spc,
                          iplayers = map (map jPt2iPt) vpts
                        }

-- Converts from our Internal Board rep to a JSON Board rep
toJBoard   :: IBoard -> JBoard
toJBoard IBoard{iturn    = trn,
                ispaces  = spc,
                iplayers = plrs} = jBoard
  where rebuildWalls = map(map (\a -> if a == gIWallHeight then gJWallHeight else a))
        jBoard = JBoard { turn    = if trn == -1 then Nothing else Just trn,
                          spaces  = if trn == -1 then Nothing else Just $ rebuildWalls spc,
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

-- Tokens for reading from a board in a robust manner.
-- Turn decisions are often determined in terms of a
-- neighborhood, rather than an absolute position. We
-- have to preserve location information, so they can
-- be mapped back to the board (except for walls, which
-- are immutable)
--
-- NOTE: This was originally going to be _just_ for determining
--       legal turns, but it's proved useful enough that
--       I upgraded it and am now using it everywhere.
data BrdTok where
  -- A given space, parameterized by location and height.
  Space  :: IPt -> Int -> BrdTok
  -- A player token, parameterized by location and height.
  Player :: IPt -> Int -> BrdTok
  -- A wall. Impassable, not buildable.
  Wall   :: BrdTok
  deriving (Eq, Show)

-- Given a token, returns the height of that token.
-- Walls have a height of 5, contradicting the JBoard representation. This allows
-- us to easily fit walls into our getMoveable code, because a board height of
-- 5 can be moved neither to nor from, given there's never a height of 4.
getHeight :: BrdTok -> Int
getHeight (Space pt height)  = height
getHeight (Player pt height) = height
getHeight Wall = gIWallHeight
