module Lecture8 where

import Prelude hiding (getLine, putStr, putStrLn)

act :: IO (Char,Char)
act  = do x <- getChar
          getChar
          y <- getChar
          return (x,y)

getLine :: IO String
getLine  = do x <- getChar
              if x == '\n' then
                 return ""
              else
                 do xs <- getLine
                    return (x:xs)

-- putChar :: Char -> IO ()

-- return ()

-- void :: ()
-- void = ()

-- putChar :: Char -> IO ()

putStr       :: String -> IO ()
putStr ""     = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn s =
  do putStr s  
     putChar '\n'

strlen :: IO ()
strlen  = do putStr "Enter a string: "
             xs <- getLine
             putStr "The string has "
             putStr (show (length xs))
             putStrLn " characters"
