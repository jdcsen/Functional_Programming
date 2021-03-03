{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module SantoriniRep where

import Control.Exception
import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics

-- Global board information..
gBrdBnd = IPt 4 4 :: IPt

gJWallHeight = 4 :: Int

gIWallHeight = 5 :: Int

gMaxTower = 3 :: Int

gMaxTravel = 1 :: Int
-- Datastructures to ease working with boards:

-- Distinguish our indexing on basis of type
data IPt = IPt
  { row :: Int,
    col :: Int
  }
  deriving (Eq, Show)

-- Our board representation. Pretty simple, has a
-- turn count, arrays for spaces and players.
--
-- We have two forms of this: JBoard, which is perfectly
-- aligned to the output requirements, and IBoard, which
-- we're more free to manipulate. We provide toJBoard and
-- fromJBoard functions..
data JBoard = JBoard
  { turn :: Maybe Int,
    spaces :: Maybe [[Int]],
    players :: [[[Int]]]
  }
  deriving (Show, Generic, Eq)

instance FromJSON JBoard

instance ToJSON JBoard

-- The canonical form of an empty board, since the 
-- empty nested arrays of varying depth were getting complicated.
gJBoardEmpty =
  JBoard
    { turn = Nothing,
      spaces = Nothing,
      players = []
    }

-- All internal functions use IBoard for consistency.
-- Right now, all we do is make the player points
-- zero-indexed so we can use them to index into the board.
data IBoard = IBoard
  { iturn :: Int,
    ispaces :: [[Int]],
    iplayers :: [[IPt]]
  }
  deriving (Show, Generic, Eq)

gIBoardEmpty =
  IBoard
    { iturn = -1,
      ispaces = replicate  (row gBrdBnd) $
                  replicate (col gBrdBnd) 0,
      iplayers = []
    }

-- Converts from a JSON Board rep to our Internal Board rep
fromJBoard :: JBoard -> IBoard
fromJBoard
  JBoard
    { turn = trn,
      spaces = spc,
      players = plrs
    } = iBoard
    where
      rebuildWalls = map (map (\a -> if a == gJWallHeight then gIWallHeight else a))
      t = fromMaybe (iturn gIBoardEmpty) trn
      s = rebuildWalls $ fromMaybe (ispaces gIBoardEmpty) spc
      p = map (map jPt2iPt) plrs
      iBoard =
        IBoard
          { 
            iturn    = t,
            ispaces  = s,
            iplayers = p
          }

-- Converts from our Internal Board rep to a JSON Board rep
toJBoard :: IBoard -> JBoard
toJBoard
  IBoard
    { iturn = trn,
      ispaces = spc,
      iplayers = plrs
    } = jBoard
    where
      rebuildWalls = map (map (\a -> if a == gIWallHeight then gJWallHeight else a))
      jBoard =
        JBoard
          { turn = if trn == -1 then Nothing else Just trn,
            spaces = if trn == -1 then Nothing else Just $ rebuildWalls spc,
            players = map (map iPt2jPt) plrs
          }

-- In order to properly feed JSON back into other AIs, we need to flip the player
-- tokens once we're done with them
--   NOTE: Still need to get single-element replacement working for record syntax.
--         This is extremely fragile wrt. changes in representation
flipPlayers :: JBoard -> JBoard
flipPlayers
  JBoard
    { turn = trn,
      spaces = spc,
      players = plrs
    } = newBrd
    where
      newPlrs = case plrs of
        [a, b] -> [b, a]
        [a] -> [a]
      newBrd =
        JBoard
          { turn = trn,
            spaces = spc,
            players = plrs
          }

-- TODO: Parameterize isFullBoard by board type and move it here.
isFullJBoard :: JBoard -> Bool
isFullJBoard
  JBoard
    { turn = Nothing,
      spaces = Nothing,
      players = _
    } = False
isFullJBoard _ = True

-- Our AI's are defined in terms of AI Kernels, which just
-- map IBoards to new IBoards.
type AIKernel = IBoard -> IBoard

-- Conversion functions.
iPt2jPt :: IPt -> [Int]
iPt2jPt bpt = [row bpt + 1, col bpt + 1]

jPt2iPt :: [Int] -> IPt
jPt2iPt (row : col : xs) = IPt (row -1) (col -1)

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
  Space :: IPt -> Int -> BrdTok
  -- A player token, parameterized by location and height.
  Player :: IPt -> Int -> BrdTok
  -- A wall. Impassable, not buildable.
  Wall :: BrdTok
  deriving (Eq, Show)

isSpace :: BrdTok -> Bool
isSpace (Space _ _) = True
isSpace _ = False

isPlayer :: BrdTok -> Bool
isPlayer (Player _ _) = True
isPlayer _ = False

isWall :: BrdTok -> Bool
isWall Wall = True
isWall _ = False

-- TODO **********************************************************
--
--  Exception throwing toSpace, toPlayer, and toWall methods.
-- NOTE: This replaceNth function comes from the selected answer to this
-- StackOverflow question. It may be replaced by tools from the 'lens' package
-- in the future:
-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
-- Answered by username Philip JF, edited by user adius
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n -1) newVal xs
