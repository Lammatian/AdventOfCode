module Main where

import System.Environment
import System.IO
import Debug.Trace

import Utility as U

sol1 :: String -> Int
sol1 _ = 0

sol2 :: String -> Int
sol2 _ = 0

main :: IO ()
main = do
        [filename] <- getArgs
        handle     <- openFile filename ReadMode
        contents   <- hGetContents handle
        print $ sol1 contents
        print $ sol2 contents