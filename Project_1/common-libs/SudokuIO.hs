{-# LANGUAGE GADTs #-}

module SudokuIO where
import FTransforms
import Transforms
import System.IO
import Data.List
import Data.Monoid
import Data.Char
import Data.Maybe

-- Our sudokuIO is done in the terms of transforms: Reading the board returns
-- an IO action and a Canonical form.
readBoard :: String -> IO FTransform
readBoard filename =
  do handle <- openFile filename ReadMode
     let canonizer = foldr (<>) (FTag CanonForm)
         shifter =
          map (\(r, t) ->
                 t <> FUnit (FUnitR Shift (TFCoord r 0 0)))
         zip_idx = zip ([0..] :: [Int])
         filt_comments = filter (isNothing . fmeta)
         stop_on_eof = takeWhile (\tform ->
                                   ftag tform == Just Identity)
         repeater = repeat
         parser = canonizer .
                  shifter .
                  zip_idx .
                  filt_comments .
                  stop_on_eof .
                  repeater
     contents <- hGetContents handle
     parser <$> sReadLine handle

sReadLine :: Handle -> IO FTransform
sReadLine handle =
  do end <- isEOF
     if end then return $ FTag Identity
     else do lineType <- hGetChar handle
             if lineType == '%' || lineType == '-'
             then sParseComment handle
             else sParseTform handle

sParseTform :: Handle -> IO FTransform
-- Are y'all ready for this
sParseTform handle = parser <$> hGetLine handle
  where grouped = groupBy (\a b -> isDigit a && isDigit b)
        filtered = filter (isDigit . head ) :: ([[Char]] -> [[Char]])
        typed = map (\a -> read a :: Int)
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

sParseComment :: Handle -> IO FTransform
sParseComment handle = FMeta <$> hGetLine handle

-- VP/VN wrappers
readBoardVP :: String -> IO Transform
readBoardVP filename = do VP . VPT <$> readBoard filename
readBoardVN :: String -> IO Transform
readBoardVN filename = do VN . VNT . VPT <$> readBoard filename
