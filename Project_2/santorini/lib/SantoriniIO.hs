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
        putStrLn outLine
        -- Flush standard out.
        SIO.hFlush SIO.stdout
  where parPline = kernelPipeline kernel

-- Given a kernel and a String, attempts to deserialize the String into a JBoard,
-- run the kernel, and return the output, ready for printing.
kernelPipeline :: AIKernel -> String -> Maybe String
kernelPipeline kernel str = newStr
  where newBoard = flipPlayers . toJBoard .
                   kernel .
                   fromJBoard <$> fromBuffer str :: Maybe JBoard
        newStr   = toBuffer <$> newBoard


toBuffer :: JBoard -> String
toBuffer brd
  | isFullJBoard brd = BL.unpack . encode $ brd
  | otherwise        = BL.unpack . encode $ players brd

fromBuffer :: String -> Maybe JBoard
fromBuffer buf
  | isJust fullDcde = fullDcde
  | isJust plrs     = Just JBoard {turn    = Nothing,
                                   spaces  = Nothing,
                                   players = fromJust plrs}
  | buf == "[[]]" || --NOTE: This is a hack. Fix it.
    buf == "{[]}"   = Just JBoard {turn    = Nothing,
                                   spaces  = Nothing,
                                   players = [[]]}

  | otherwise        = Nothing
  where fullEDecode  = eitherDecode . BL.pack $ buf :: Either String JBoard
        fullDcde     = either (const Nothing) Just fullEDecode
        fullDedeErr  = fromLeft "" fullEDecode

        p2EDcder = eitherDecode . BL.pack $ buf :: Either String [[JPt]]
        p2Dcder  = decode . BL.pack :: String -> Maybe [[JPt]]
        plrs     = p2Dcder buf
