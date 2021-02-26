{-# LANGUAGE GADTs #-}

module SantoriniIO where

import SantoriniRep
import Control.Monad
import Control.Monad.ST.Lazy
import Control.Exception
import Data.Aeson
import Data.Maybe
import Data.Char
import Data.Either
import qualified System.IO as SIO
import qualified Data.ByteString.Lazy.Char8 as BL

-- Processes a string of characters, returning substrings of self-delimiting
-- JSON, returning leftovers. If no self-delimiting JSON is in the string,
-- the first element of the return tuple will be empty. If there are any leading
-- characters, they'll be trimmed.

kernelRunner :: AIKernel -> IO ()
kernelRunner kernel = forever $ do
        line <- getLine
        let outLine = case parPline line
                        of (Just str) -> str
                           Nothing    -> error "Failed to parse line: " ++ line

        -- Flush standard out.
        SIO.hFlush SIO.stdout
  where parPline = kernelPipeline kernel

-- Given a kernel and a String, attempts to deserialize the String into a JBoard,
-- run the kernel, and return the output, ready for printing.
kernelPipeline :: AIKernel -> String -> Maybe String
kernelPipeline kernel str = newStr
  where newBoard = flipPlayers . toJBoard .
                   kernel .
                   fromJBoard <$> deserJBoard str :: Maybe JBoard
        newStr   = toBuffer <$> newBoard


-- Attempts to extract an JBoard from the string. Discards trailing and leading
-- characters. If there is more than one board, only the first is returned.
deserJBoard :: [Char] -> Maybe JBoard
deserJBoard str = board
  where -- Deserialization Pipeline
        scan   = scanl putBuffer NoBuffer
        filt   = filter (isJust . bufToJBoard)
        mapper = map bufToJBoard
        boardList = mapper . filt . scan $ str
        board = case boardList of [] -> Nothing
                                  (x:_) -> x

-- JSONBuffers are just character accumulators so we can chunk up our JSON before
-- sending it off to our serialization/deserialization library.
data JSONBuffer where
  SaturatedBuffer :: [Char] -> JSONBuffer
  FillingBuffer   :: (Char, Char) -> [Char] -> JSONBuffer
  NoBuffer        :: JSONBuffer
  deriving (Show, Eq)

isSaturated :: JSONBuffer -> Bool
isSaturated (SaturatedBuffer _) = True
isSaturated _ = False

toBuffer :: JBoard -> String
toBuffer = BL.unpack . encode

bufToJBoard :: JSONBuffer -> Maybe JBoard
bufToJBoard (SaturatedBuffer buf) = decode $ BL.pack buf
bufToJBoard _ = Nothing

-- Puts a single character in a JSONBuffer.
putBuffer :: JSONBuffer -> Char -> JSONBuffer

-- Saturated Buffers act as NoBuffers
putBuffer (SaturatedBuffer buf) head = putBuffer NoBuffer head

-- Filling buffers fill until their delimeters are hit.
putBuffer (FillingBuffer (ldelim, rdelim) buf) head
  | head == rdelim = SaturatedBuffer ([ldelim] ++ buf ++ [rdelim])
  | otherwise         = FillingBuffer (ldelim, rdelim) (buf ++ [head])

-- We try and start filling a new buffer with a NoBuffer
putBuffer NoBuffer head
  | head == '[' = FillingBuffer ('[', ']') []
  | head == '{' = FillingBuffer ('{', '}') []
  | otherwise   = NoBuffer
