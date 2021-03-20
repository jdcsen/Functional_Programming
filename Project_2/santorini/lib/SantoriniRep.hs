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

gMaxTravelUp = 1  :: Int

-- Datastructures to ease working with boards:

-- Distinguish our indexing on basis of type
data IPt = IPt
  { row :: Int,
    col :: Int
  }
  deriving (Eq, Show)

-- Card types
data CardE =
  Apollo     |
  Artemis    |
  Atlas      |
  Demeter    |
  Hephastus  |
  Minotaur   |
  Pan        |
  Prometheus deriving (Enum, Show, Generic, Eq)

-- Associate each enum with a string so we can search both ways, instead of
-- having two conversion tables.
gCardStPairs =
  [ ("Apollo",     Apollo),
    ("Artemis",    Artemis),
    ("Atlas",      Atlas),
    ("Demeter",    Demeter),
    ("Hephastus", Hephastus),
    ("Minotaur",   Minotaur),
    ("Pan",        Pan),
    ("Prometheus", Prometheus)
  ]

-- Convert a card to a string. Fails on error.
cardToStr :: CardE -> String
cardToStr card = fromJust $ fst <$> find (\a -> snd a == card) gCardStPairs

-- Convert a string to a card. Fails on error.
strToCard :: String -> CardE
strToCard card = fromJust $ snd <$> find (\a -> fst a == card) gCardStPairs


-- Player structs, added as a result of the addition to cards.
-- Have J and I versions.

-- JPlayer matches the format of the input exactly
data JPlayer = JPlayer
  { card   :: String,
    tokens :: Maybe [[Int]]
  } deriving (Show, Generic, Eq)

instance FromJSON JPlayer

instance ToJSON JPlayer where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

-- The IPlayer is identical to a JPlayer, but validated and 0 indexed.
data IPlayer = IPlayer
  { icard   :: CardE,
    itokens :: [IPt]
  } deriving (Show, Generic, Eq)

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
    players :: [JPlayer]
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
    iplayers :: [IPlayer]
  }
  deriving (Show, Generic, Eq)

gIBoardEmpty =
  IBoard
    { iturn = -1, -- Note: +1's are due to zero based indexing.
      ispaces = replicate  (row gBrdBnd + 1) $
                  replicate (col gBrdBnd + 1) 0,
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
      p = map fromJPlayer plrs
      iBoard =
        IBoard
          {
            iturn    = t,
            ispaces  = s,
            iplayers = p
          }

-- Converts from a JPlayer to an IPlayer
fromJPlayer :: JPlayer -> IPlayer
fromJPlayer
  JPlayer
    { card = crd,
      tokens = tkns
    } = iPlayer
  where
    mappedToks = maybe [] (map jPt2iPt) tkns
    iPlayer =
      IPlayer
        { icard   = strToCard crd, --Default to Artemis
          itokens = mappedToks
        }


-- Converts from an IPlayer to a JPlayer
toJPlayer :: IPlayer -> JPlayer
toJPlayer
  IPlayer
    { icard = crd,
      itokens = tkns
    } = jPlayer
  where
    mappedToks = map iPt2jPt tkns
    newToks = case mappedToks of [] -> Nothing
                                 _  -> Just mappedToks
    jPlayer =
      JPlayer
        { card   = cardToStr crd, --Default to Artemis
          tokens = newToks
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
            players = map toJPlayer plrs
          }

-- In order to properly feed JSON back into other AIs, we need to flip the player
-- tokens once we're done with them
--   NOTE: Still need to get single-element replacement working for record syntax.
--         This is extremely fragile wrt. changes in representation
flipPlayers :: IBoard -> IBoard
flipPlayers
  IBoard
    { iturn = trn,
      ispaces = spc,
      iplayers = plrs
    } = newBrd
    where
      newPlrs = case plrs of
        [a, b] -> [b, a]
        [a] -> [a]
      newBrd =
        IBoard
          { iturn = trn,
            ispaces = spc,
            iplayers = newPlrs
          }

-- Increment the turn number
incrementTurn :: IBoard -> IBoard
incrementTurn
  IBoard
    { iturn = trn,
      ispaces = spc,
      iplayers = plrs
    } = newBrd
    where
      -- TODO: I _KNOW_ there's a better way to do this.
      newBrd =
        IBoard
          { iturn = if trn == -1 then -1 else trn + 1,
            ispaces = spc,
            iplayers = plrs
          }

-- Error checking function to get our current player.
getOurPlayer :: IBoard -> IPlayer
getOurPlayer brd = players
  where
    players = case iplayers brd of
      (pa: pas) -> pa
      _         -> error  "getOurPlayers: No players."

-- Error checking function to get the other player.
getOtherPlayer :: IBoard -> IPlayer
getOtherPlayer brd = players
  where
    players = case iplayers brd of
      (pa: pb: pas) -> pb
      _         -> error "getOurPlayers: Fewer than two players."

-- TODO: Parameterize isFullBoard by board type and move it here.
isFullJBoard :: JBoard -> Bool
isFullJBoard
  JBoard
    { turn = Nothing,
      spaces = Nothing,
      players = _
    } = False
isFullJBoard _ = True

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
  Space  :: IPt -> Int -> BrdTok
  -- A player token, parameterized by location and height.
  Player :: IPt -> Int -> BrdTok
  -- A wall. Impassable, not buildable.
  Wall   :: IPt -> BrdTok
  deriving (Eq, Show)

isSpace :: BrdTok -> Bool
isSpace (Space _ _) = True
isSpace _ = False

isPlayer :: BrdTok -> Bool
isPlayer (Player _ _) = True
isPlayer _ = False

isWall :: BrdTok -> Bool
isWall (Wall _) = True
isWall _ = False

getLoc :: BrdTok -> IPt
getLoc (Space loc _)  = loc
getLoc (Player loc _) = loc
getLoc (Wall loc)     = loc

-- TODO **********************************************************
--
--  Exception throwing toSpace, toPlayer, and toWall methods.
