module SantoriniLogic where
import SantoriniRep

-- Determines whether or not the board is in a setup state.
isStartingState :: IBoard -> Bool
isStartingState IBoard{iturn   = Nothing,
                       ispaces  = Nothing,
                       iplayers = playArr} = True
isStartingState _ = False

-- Determines whether or not the board is in a play state.
isFullBoard :: IBoard -> Bool
isFullBoard IBoard{iturn   = Just turn,
                   ispaces  = Just spaces,
                   iplayers = playArr} = True
isFullBoard _ = False

-- Given a position, returns the proximity of that position.
-- Move decisions are made in terms of proximity. Cells are ordered clockwise
-- starting from the top left.
getProx :: IBoard-> iPt -> [BrdTok]
getProx _ _ = [] --TODO: Implement.
