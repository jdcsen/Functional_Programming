module Main where
import MainWrappers (preludeIO, postIO)

main :: IO ()
main = do input <- preludeIO
          postIO (input ++ "bullshit")

