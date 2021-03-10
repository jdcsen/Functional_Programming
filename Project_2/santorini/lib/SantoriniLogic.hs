{-# LANGUAGE LambdaCase #-}

module SantoriniLogic where
import Control.Exception
import Data.Data
import Data.List
import Data.Maybe
import SantoriniRep

-- Determines whether or not the board is in a play state.
isFullBoard :: IBoard -> Bool
isFullBoard
  IBoard
    { iturn = -1,
      ispaces = _,
      iplayers = playArr
    } = False
isFullBoard _ = True

-- Determines whether or not the board has been statically won
isStaticWon :: IBoard -> Bool
-- Starting boards are never won.
isStaticWon
  IBoard
    { iturn = -1,
      ispaces = _,
      iplayers = _
    } = False

isStaticWon brd = isWon
    where plrTkns = case iplayers brd of
            [a,b] -> itokens a ++ itokens b
            _ -> throw $ UndefinedElement "Tried to check the win state of a board with missing players."
          locs = map (getHeight . getTok brd) plrTkns
          isWon = gMaxTower `elem`  locs

-- Given a board and a point, returns the Board Token at that point. Does not
-- have to respect the boundaries of the board Array, anything out of range is a Wall
getTok :: IBoard -> IPt -> BrdTok
getTok brd pt
  | out_of_range = Wall pt
  | height >= gIWallHeight = Wall pt
  | has_player = Player pt height
  | otherwise = Space pt height
  where
    -- I thought this was clever. Rows and columns need
    -- to be checked against each other, so I parameterized
    -- by the functions themselves.
    out_of_range =
      any
        (\a -> a pt > a gBrdBnd || a pt < 0)
        [row, col]
    ptelem = elem pt :: [IPt] -> Bool
    has_player = any (ptelem . itokens) $ iplayers brd
    height =
      if not out_of_range
        then (ispaces brd !! row pt) !! col pt
        else gIWallHeight

-- Given a position, returns the proximity of that position.
-- Move decisions are made in terms of proximity. Cells are ordered clockwise
-- starting from the top left.
getProx :: IBoard -> IPt -> [BrdTok]
getProx brd pt = vals
  where
    osets = [1, 0, -1]
    points =
      [ IPt
          (row pt - row_oset)
          (col pt - col_oset)
        | row_oset <- osets,
          col_oset <- osets
      ]
    vals = map (getTok brd) points

-- Given a board and a position, returns all the buildable tiles near that position.
getBuildable :: IBoard -> IPt -> [BrdTok]
getBuildable brd pt = buildable
  where
    predicate = \case
      (Space _ _) -> True
      _ -> False
    filt = filter predicate
    buildable = filt $ getProx brd pt

-- Given a board and a position, returns all the movable tiles near that position.
getMoveable :: IBoard -> IPt -> [BrdTok]
getMoveable brd pt = moveable
  where
    hght = getHeight $ getTok brd pt
    predicate = \case
      (Space _ shght) -> abs (hght - shght) <= gMaxTravel
      _ -> False
    filt = filter predicate
    moveable = filt $ getProx brd pt

-- Given a token, returns the height of that token.
-- Walls have a height of 5, contradicting the JBoard representation. This allows
-- us to easily fit walls into our getMoveable code, because a board height of
-- 5 can be moved neither to nor from, given there's never a height of 4.
getHeight :: BrdTok -> Int
getHeight (Space  _ height) = height
getHeight (Player _ height) = height
getHeight (Wall   _       ) = gIWallHeight

-- Given a token, returns the position of that token.
getPos :: BrdTok -> IPt
getPos (Space  pt _) = pt
getPos (Player pt _) = pt
getPos (Wall   pt)   = pt

-- Gets the IPlayer that owns the specified player location, with an error message
-- if the location is not a player.
getPlayer :: IBoard -> IPt -> String -> IPlayer
getPlayer brd loc errMsg = player
  where
    player = case filter (\a -> loc `elem` itokens a) $ iplayers brd of
              [player] -> player
              _ -> throw $ UndefinedElement errMsg


-- Moves a player from a source location to a target location.
-- If no player is at the source location, throws UndefinedElement
movePlayer :: IBoard -> (IPt, IPt) -> IBoard
movePlayer brd (src, tgt) = newBrd
  where
    oldPlayer = getOurPlayer brd
    oldToks = itokens oldPlayer
    -- Ensure we have a player to move.
    vsrc =
      if src `elem` oldToks
        then src
        else throw $ UndefinedElement "The source player does not exist."

    -- Replace the positions
    newToks = tgt : delete vsrc oldToks
    newPlayer =
      IPlayer
        { icard = icard oldPlayer,
          itokens = newToks
        }

    -- Replace the player.
    newPlayers = newPlayer : delete oldPlayer (iplayers brd)

    -- Rebuild the board.
    --   Note: functions for single element replacement? Does record syntax
    --         give us anything like that?
    newBrd =
      IBoard
        { iturn = iturn brd,
          ispaces = ispaces brd,
          iplayers = newPlayers
        }

-- Builds a level at the target location.
-- If the element at the location is not buildable, throws UndefinedElement
buildLvl :: IBoard -> IPt -> IBoard
buildLvl brd loc = newBrd
  where
    -- Ensure we can build at this location.
    -- NOTE: This does boundary checking for us, because out of bounds indexes are walls.
    validHeight = case getTok brd loc of
      (Space loc ht) -> ht
      _ -> throw $ UndefinedElement "Tried to build on an invalid space."

    -- NOTE: We check for walls and players above, so this is safe.
    newHeight =
      if validHeight == gMaxTower
        then gIWallHeight
        else validHeight + 1

    -- Update the spaces array.
    oldSpaces = ispaces brd
    oldRow = oldSpaces !! row loc
    -- Split at the column. This gives us a head list that can remain unchanged,
    -- and a tail list that has our value at the head.
    (headL, tailL) = splitAt (col loc) oldRow
    newTailL = drop 1 tailL
    newRow = headL ++ [newHeight] ++ newTailL
    -- Do the same for the entire spaces array.
    (spHeadL, spTailL) = splitAt (row loc) oldSpaces
    newSpTailL = drop 1 spTailL
    newSpaces = spHeadL ++ [newRow] ++ newSpTailL

    -- Rebuild the board.
    newBrd =
      IBoard
        { iturn = iturn brd,
          ispaces = newSpaces,
          iplayers = iplayers brd
        }

-- Places a player at the target location.
-- If the element at the location cannot have a player placed, throws UndefinedElement
placePlayer :: IBoard -> IPt -> IBoard
placePlayer brd loc = newBrd
  where
    -- Ensure we can place a player at this location.
    vloc = case getTok brd loc of
      (Space loc ht) -> loc
      _ -> throw $ UndefinedElement "Tried to place a player on an invalid space."

    -- Grab the player.
    oldP = case iplayers brd of
      [] -> throw $ UndefinedElement "Tried to place a player with no player data."
      [p1] -> p1
      [p1, p2] -> p1

    -- Append location.
    newToks = loc : itokens oldP

    -- Rebuild the player
    newP =
      IPlayer
        { icard   = icard oldP,
          itokens = newToks
        }

    -- Rebuild the player array.
    newPlayers = newP : delete oldP (iplayers brd)

    -- Rebuild the board.
    newBrd =
      IBoard
        { iturn = iturn brd,
          ispaces = ispaces brd,
          iplayers = newPlayers
        }

-- Swaps two player locations.
-- If there aren't players at both locations, throws UndefinedElement
--
-- Swapped players must be on different teams. This requirement is posed purely
-- for ease of implementation, but there are no known cases where we need to swap
-- players on the same team, so YNGNI
swapPlayer :: IBoard -> (IPt, IPt) -> IBoard
swapPlayer brd (l1, l2) = newBrd
  where
    -- Grab the players.
    p1 = getPlayer brd l1 "Swap Player: First location is not a player."

    potPlayer = getPlayer brd l2 "Swap Player: Second location is not a player."
    p2 = if p1 == potPlayer
         then throw $ UndefinedElement "Swap Player: Both players are on the same team."
         else potPlayer

    -- Swap the players.
    newP1 =
      IPlayer
        { icard = icard p1,
          itokens = l2 : delete l1 (itokens p1)
        }

    newP2 =
      IPlayer
        { icard = icard p2,
          itokens = l1 : delete l2 (itokens p2)
        }

    -- Rebuild the players, preserving order.
    newPlayers = if p1 == head (iplayers brd)
                    then newP1 : delete p1 (newP2 : delete p2 (iplayers brd))
                    else newP2 : delete p2 (newP1 : delete p1 (iplayers brd))

    -- Rebuild the board.
    newBrd =
      IBoard
        { iturn = iturn brd,
          ispaces = ispaces brd,
          iplayers = newPlayers
        }

-- Moves a player, pushing the second player directly backwards.
-- NOTE: Because of the mechanics of this transformation, this operation has
--       slightly tighter constraints than all of our other board mutation methods.
--       Players are required to be directly adjacent in order to push other players.
--       This is required because a movement that was the result of a push is
--       not a move, but rather a "force". Both movements must be applied atomically,
--       so we don't encounter a false "win" state mid-mutation. The adjacency
--       requirement is required to determine the direction of the force.
--
--       If the players are not adjacent, throws UndefinedElement.
--
--       If either the move or the force move is fundamentally invalid (i.e: a
--       movement to another player or a wall), throws UndefinedElement
pushPlayer :: IBoard -> (IPt, IPt) -> IBoard
pushPlayer brd (l1, l2) = newBrd
  where
    -- Ensure the two pieces are adjacent by pulling player tokens from the
    -- proximity function.
    proxPred searchLoc = (searchLoc ==) . getLoc

    -- Note: Currently, our sense of proximity is symmetric. If one of these fails,
    -- the next will never run. If the first succeeds, the second will succeed.
    t1 = case filter (proxPred l1) (getProx brd l2) of
           [player] -> player
           _ -> throw $ UndefinedElement "Push Player: Locations are not proximal (t1->t2)."

    t2 = case filter (proxPred l2) (getProx brd l1) of
           [player] -> if t1 == player
                       then throw $ UndefinedElement "Push Player: Both players are at the same location."
                       else player
           _ -> throw $ UndefinedElement "Push Player: Locations are not proximal (t2->t1)."

    -- Grab the players.
    p1 = getPlayer brd (getLoc t1) "Push Player: First location is not a player."

    potPlayer = getPlayer brd (getLoc t2) "Push Player: Second location is not a player."
    p2 = if p1 == potPlayer
         then throw $ UndefinedElement "Push Player: Both players are on the same team."
         else potPlayer

    -- First, force the second player out of the way.
    (rowDelta, colDelta) = (row l2 - row l1, col l2 - col l1)
    newl2 = IPt (row l2 + rowDelta) (col l2 + colDelta)

    -- Rebuild the players.
    newP1 =
      IPlayer
        { icard = icard p1,
          itokens = l2 : delete l1 (itokens p1)
        }

    newP2 =
      IPlayer
        { icard = icard p2,
          itokens = newl2 : delete l2 (itokens p2)
        }

    -- TODO: I do this twice. replacePlayer function?
    -- Rebuild the players, preserving order.
    newPlayers = if p1 == head (iplayers brd)
                    then newP1 : delete p1 (newP2 : delete p2 (iplayers brd))
                    else newP2 : delete p2 (newP1 : delete p1 (iplayers brd))

    -- Rebuild the board.
    newBrd =
      IBoard
        { iturn = iturn brd,
          ispaces = ispaces brd,
          iplayers = newPlayers
        }


