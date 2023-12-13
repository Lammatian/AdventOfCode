module Lib
    ( printGrid
    ) where

printGrid :: [String] -> IO ()
printGrid [] = putStrLn ""
printGrid (s:ss) =
  do
    putStrLn s
    printGrid ss