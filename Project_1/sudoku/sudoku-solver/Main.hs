module Main where

import System.Environment
import Data.Map
import FTransforms
import SudokuIO

--mFromList = Data.Map.fromList

main :: IO ()
main = do
  filename <- head <$> getArgs
  board <- readBoard filename
  putStr $ show (solveBoard board)
--where
--  pmatch = mFromList
--    [("-v", Nothing), --Solve the board at the filename
--     ("-g", Nothing)  --Generate n boards
--    ]
--  match_args = map (
--  parser = id

solveBoard :: FTransform -> FTransform
solveBoard ft = ft
