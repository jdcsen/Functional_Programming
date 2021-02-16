module MainWrappers where

preludeIO :: IO String
preludeIO = do putStrLn "Prelude IO"
               retStr <- getLine
               return retStr

postIO :: String -> IO ()
postIO endStr =
  putStrLn ("Post IO" ++ endStr)
