
module SimpleSudoku where

import Data.Array
import System.IO
import Data.Char
import Data.String
import Control.Exception

type Board = Array Int (Array Int (Maybe Int))

-- Views for rows, columns, and boxes
-- NOTE: Fixed to 3x3. Because this is simple.
vRow :: Board -> Int -> Array Int (Maybe Int)
vRow board row = board ! row

vCol :: Board -> Int -> Array Int (Maybe Int)
vCol board col = fmap (! col) board

vBox :: Board -> Int -> Array Int (Maybe Int)
vBox board idx = box
  where nrows = 3 :: Int
        ncols = 3 :: Int
        row = div idx ncols
        col = mod idx nrows
        min_row = row * nrows
        min_col = row * ncols
        -- Todo: filter columns from rows, then rows from columns.
        box = listArray (0,0) []

solveBoard :: Board -> Maybe Board
solveBoard = Just -- Todo: Implement

readLine :: String -> Array Int (Maybe Int)
readLine inLine = arr
  where
        filt = filter (\str -> "" /= str &&
                        isDigit (head str) ||
                        '.' == head str)
        typed = map (\a -> if head a /= '.'
                           then Just (read a)
                           else Nothing :: Maybe Int)
        parser = typed . filt . words
        values = parser inLine
        arr = listArray (0, length values - 1) values

readBoard :: String -> IO Board
readBoard filename =
  do handle <- openFile filename ReadMode
     rows <- parser <$> hGetContents handle
     return $ listArray (0, length rows - 1) rows
  where parse_line = map readLine
        filt = filter (\str -> '%' /= head str &&
                               '-' /= head str)
        stop_on_eof = takeWhile (/= "") :: [String] -> [String]
        parser = parse_line .
                 filt .
                 stop_on_eof .
                 lines

