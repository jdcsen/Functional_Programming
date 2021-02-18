{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module SantoriniIO where

import GHC.Generics
import Data.Aeson

data SBoard = SBoard { turn :: Int,
                       spaces :: [[Int]],
                       players :: [[Int]]
                     } deriving (Show, Generic)

instance FromJSON SBoard
instance ToJSON   SBoard

-- Processes a string of characters, returning substrings of self-delimiting
-- JSON, returning leftovers. If no self-delimiting JSON is in the string,
-- the first element of the return tuple will be empty. If there are any leading
-- characters, they'll be trimmed.

-- JSONBuffers are just character accumulators so we can chunk up our JSON before
-- sending it off to our serialization/deserialization library.
data JSONBuffer where
  FillingBuffer   :: (Char, Char) -> [Char] -> JSONBuffer
  SaturatedBuffer :: (Char, Char) -> [Char] -> JSONBuffer
  NoBuffer        :: JSONBuffer
  deriving (Show, Eq)

isSaturated :: JSONBuffer -> Bool
isSaturated (SaturatedBuffer _ _) = True
isSaturated _ = False

jsonStreamIO :: IO [JSONBuffer]
jsonStreamIO = jsonStream <$> getContents

-- When passed a list, scans the list of chars and returns a list of Saturated JSON buffers.
jsonStream :: [Char] -> [JSONBuffer]
jsonStream = filt.scanner
  where scanner = scanl fillBuffer NoBuffer
        filt    = filter isSaturated

-- Fills a (single) JSON buffer from the stream of characters. Can either fail
-- with NoBuffer, partially succeed with FillingBuffer , or return a filled buffer
-- with SaturatedBuffer. Used in the jsonStream function.
fillBuffer :: JSONBuffer -> Char -> JSONBuffer

-- Saturated Buffers act as NoBuffers
fillBuffer (SaturatedBuffer delim buf) head = fillBuffer NoBuffer head

-- Filling buffers fill until their delimeters are hit.
fillBuffer (FillingBuffer delim buf) head
  | head == snd delim = SaturatedBuffer delim buf
  | otherwise         = FillingBuffer delim (buf ++ [head])

-- We try and start filling a new buffer with a NoBuffer
fillBuffer NoBuffer head
  | head == '[' = FillingBuffer ('[',']') []
  | head == '{' = FillingBuffer ('{','}') []
  | otherwise   = NoBuffer

