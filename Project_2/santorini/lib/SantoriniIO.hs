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

-- The first handle is the input, the second the output.
kernelRunner :: SIO.Handle -> SIO.Handle -> AIKernel -> IO ()
kernelRunner inHdl outHdl kernel = forever $ do
  line <- SIO.hGetLine inHdl
  let outLine = case parPline line of
        (Right str) -> str
        (Left err) -> error "Failed to parse line: " ++ line ++ ", Error: " ++ err
  SIO.hPutStrLn outHdl outLine
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
      toBuffer . incrementTurn . flipPlayers . toJBoard
        . kernel
        . fromJBoard
        $ valid ::
        String

fromBufferStart :: String -> Either SErr JBoard
fromBufferStart buf
  | buf == "" = Right gJBoardEmpty
  | isRight plrsE =
    Right
      JBoard
        { turn = Nothing,
          spaces = Nothing,
          players = plrs
        }
  | otherwise = Left plrsErr
  where
    plrsE = eitherDecode . BL.pack $ buf :: Either String [JPlayer]
    plrsErr = fromLeft "" plrsE
    plrs = fromRight [] plrsE

fromBufferFull :: String -> Either SErr JBoard
fromBufferFull buf = eitherDecode . BL.pack $ buf :: Either String JBoard

toBuffer :: JBoard -> String
toBuffer brd
  | isFullJBoard brd = BL.unpack . encode $ brd
  | otherwise = BL.unpack . encode $ players brd

fromBuffer :: String -> Either SErr JBoard
fromBuffer buf
  | isRight full = fromBufferFull buf
  | isRight start = fromBufferStart buf
  | otherwise = Left err_msg
  where
    full = fromBufferFull buf
    fullErr = fromLeft "" full
    start = fromBufferStart buf
    startErr = fromLeft "" start

    err_msg = "None of the decoders matched. Full Error: " ++ fullErr ++ " Starter Error: " ++ startErr
