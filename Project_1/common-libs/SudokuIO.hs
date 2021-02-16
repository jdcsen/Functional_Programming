{-# LANGUAGE GADTs #-}

module SudokuIO where
import FTransforms
import Transforms
import System.IO
import Data.List
import Data.Monoid
import Data.Char
import Data.Maybe
import Control.Exception

rBoardHandler :: IOError -> IO String
rBoardHandler e = return ""

-- Our sudokuIO is done in the terms of transforms: Reading the board returns
-- an IO action and a Canonical form.
readBoard :: String -> IO FTransform
readBoard filename =
  do handle <- openFile filename ReadMode
     parser <$> hGetContents handle
  where canonizer = foldr (<>) (FTag CanonForm)
        shifter =
         map (\(r, t) ->
                t <> FUnit (FUnitR Shift (TFCoord r 0 0)))
        zip_idx = zip ([0..] :: [Int])
        filt_comments = filter (isNothing . fmeta)
        parse_line = map sParseLine
        stop_on_eof = takeWhile (/= "") :: [String] -> [String]
        repeater = repeat :: String -> [String]
        parser = canonizer .
                 shifter .
                 zip_idx .
                 filt_comments .
                 parse_line .
                 stop_on_eof .
                 lines

sParseLine :: String -> FTransform
sParseLine string = tform
  where lineType = head string
        tform = case lineType of '%' -> sParseComment string
                                 '-' -> sParseComment string
                                 _   -> sParseTform   string

sParseTform :: String -> FTransform
-- Are y'all ready for this
sParseTform = parser
  where grouped = words --groupBy (\a b -> isDigit a && isDigit b)
        filtered = filter (\str -> isDigit (head str) || '.' == head str)
        typed = map (\a -> if head a /= '.' then read a else 0 :: Int)
        indexed = zip ([0..] :: [Int])
        constructed =
          map (\(col, val) ->
                 FUnit (FUnitR Insert (TFCoord 0 col val)))
        -- Note: We pin to row 0 here, and expect a RShift
        --       later if one is warranted.
        -- Input and build
        parser = foldr (<>) (FTag Identity) .
                 constructed .
                 indexed .
                 typed .
                 filtered .
                 grouped

--               filter

sParseComment :: String -> FTransform
sParseComment = FMeta

-- VP/VN wrappers
readBoardVP :: String -> IO Transform
readBoardVP filename = do VP . VPT <$> readBoard filename
readBoardVN :: String -> IO Transform
readBoardVN filename = do VN . VNT . VPT <$> readBoard filename
