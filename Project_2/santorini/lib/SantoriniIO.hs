
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
import AIKernels
import qualified System.IO as SIO

-- Allows us to distinguish errors and valid string values syntactically
type SErr = String

-- Processes a string of characters, returning substrings of self-delimiting
-- JSON, returning leftovers. If no self-delimiting JSON is in the string,
-- the first element of the return tuple will be empty. If there are any leading
-- characters, they'll be trimmed.

-- The first handle is the input, the second the output.
kernelRunner :: SIO.Handle -> SIO.Handle -> Kernel -> IO ()
kernelRunner inHdl outHdl kernel = forever $ do
  -- Note to self: Am I just piggy-backing off of the IO Monad to sequence these
  -- transformations and resolve referential transparency issues? Not sure.
  line <- SIO.hGetLine inHdl
  kRes <- return $ kernelPipeline kernel line :: IO (Either SErr (Kernel, String))
  (kernel, outStr) <- return (fromRight (NullKernel, error "Parser Error") kRes)
  -- NOTE: Currently discards the parser-specific error string.
  SIO.hPutStrLn outHdl outStr
  -- Flush standard out.
  SIO.hFlush SIO.stdout

-- Given a kernel and a String, attempts to deserialize the String into a JBoard,
-- run the kernel, and return the output, ready for printing.
kernelPipeline :: Kernel -> String -> Either SErr (Kernel, String)
kernelPipeline kernel str
  | isRight parsed = Right (newK, newStr)
  | otherwise = Left errStr
  where
    parsed = fromBuffer str
    errStr = fromLeft "" parsed
    valid = fromRight gJBoardEmpty parsed
    deserBrd = fromJBoard valid
    (newK, newBrd) = tick (kernel, deserBrd)
    newStr = toBuffer . incrementTurn . flipPlayers . toJBoard $ newBrd

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
