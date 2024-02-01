module Util
    ( printGrid,
      bisect,
      bisectOn
    ) where

import Data.List.Split (splitOn)

-- Nicely print a grid
printGrid :: [String] -> IO ()
printGrid [] = putStrLn ""
printGrid (s:ss) =
  do
    putStrLn s
    printGrid ss

-- Given a string, split it into two exactly two parts on whitespace and fail otherwise
bisect :: String -> (String, String)
bisect = bisectOn " "

-- Given a string, split it into two exactly two parts on a delimiter and fail otherwise
bisectOn :: String -> String -> (String, String)
bisectOn d s = case splitOn d s of
  [x, y] -> (x, y)
  _      -> error $ "Could not split " ++ s ++ " on " ++ d ++ " into two parts"