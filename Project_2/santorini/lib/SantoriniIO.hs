{-# LANGUAGE GADTs #-}

module SantoriniIO where

import Control.Exception
import Control.Monad
import Control.Monad.ST.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char
import Data.Either
import Data.Maybe
import SantoriniRep
import qualified System.IO as SIO

-- Allows us to distinguish errors and valid string values syntactically
type SErr = String

-- Processes a string of characters, returning substrings of self-delimiting
-- JSON, returning leftovers. If no self-delimiting JSON is in the string,
-- the first element of the return tuple will be empty. If there are any leading
-- characters, they'll be trimmed.

kernelRunner :: AIKernel -> IO ()
kernelRunner kernel = forever $ do
  line <- getLine
  let outLine = case parPline line of
        (Right str) -> str
        (Left err) -> error "Failed to parse line: " ++ line ++ ", Error: " ++ err
  putStrLn outLine
  -- Flush standard out.
  SIO.hFlush SIO.stdout
  where
    parPline = kernelPipeline kernel

-- Given a kernel and a String, attempts to deserialize the String into a JBoard,
-- run the kernel, and return the output, ready for printing.
kernelPipeline :: AIKernel -> String -> Either SErr String
kernelPipeline kernel str
  | isRight parsed = Right newStr
  | otherwise = Left errStr
  where
    parsed = fromBuffer str
    errStr = fromLeft "" parsed
    valid = fromRight gJBoardEmpty parsed
    
    newStr =
      toBuffer . flipPlayers . toJBoard
        . kernel
        . fromJBoard
        $ valid ::
        String

fromBufferStart :: String -> Either SErr JBoard
fromBufferStart buf
  | isJust plrs =
    Right
      JBoard
        { turn = Nothing,
          spaces = Nothing,
          players = fromJust plrs
        }
  | buf == "[[]]"
      || buf == "{[]}" =
    Right gJBoardEmpty --NOTE: This is a hack. Fix it.
  | otherwise = Left plrsErr
  where
    decodeE = eitherDecode . BL.pack $ buf :: Either String [[JPt]]
    plrs = either (const Nothing) Just decodeE
    plrsErr = fromLeft "" decodeE

fromBufferFull :: String -> Either SErr JBoard
fromBufferFull buf = eitherDecode . BL.pack $ buf :: Either String JBoard

toBuffer :: JBoard -> String
toBuffer brd
  | isFullJBoard brd = BL.unpack . encode $ brd
  | otherwise = BL.unpack . encode $ players brd

fromBuffer :: String -> Either SErr JBoard
fromBuffer buf
  | isRight full = fromBufferFull buf
  | isRight start = fromBufferFull buf
  | otherwise = Left err_msg
  where
    full = fromBufferFull buf
    start = fromBufferStart buf
    err_msg = "None of the buffers matched."
