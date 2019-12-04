module Utility where

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (==c) s of
                    "" -> []
                    s' -> w : splitOn c s''
                            where (w, s'') = break (==c) s'

splitLines :: String -> [String]
splitLines = splitOn '\n'

readInt :: String -> Int
readInt s = read s::Int