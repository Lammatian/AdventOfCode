module Lib
    ( printGrid,
      bisect,
      bisectOn
    ) where

import Data.List.Split (splitOn)

printGrid :: [String] -> IO ()
printGrid [] = putStrLn ""
printGrid (s:ss) =
  do
    putStrLn s
    printGrid ss

bisect :: String -> (String, String)
bisect s = case words s of
  [x, y] -> (x, y)
  _      -> error $ "Could not split " ++ s ++ " into two parts"

bisectOn :: String -> String -> (String, String)
bisectOn d s = case splitOn d s of
  [x, y] -> (x, y)
  _      -> error $ "Could not split " ++ s ++ " on " ++ d ++ " into two parts"