module Main where

import System.Environment
import System.IO
import Debug.Trace
import Data.Maybe

import Utility as U

isValid :: Int -> Bool
isValid n = isValidRec n 10 False

isValidRec :: Int -> Int -> Bool -> Bool
isValidRec 0 _ pair    = pair
isValidRec n last pair
        | last < d  = False
        | not pair  = isValidRec (n `div` 10) d (d == last)
        | otherwise = isValidRec (n `div` 10) d pair
        where d = n `mod` 10

sol1 :: String -> Int
sol1 s = length [ x | x <- [b..e], isValid x ]
    where nums   = map U.readInt $ U.splitOn '-' s
          (b, e) = (head nums, nums!!1)

isValid2 :: Int -> Bool
isValid2 n = isIncreasing n && hasDouble n

hasDouble :: Int -> Bool
hasDouble n = hasDoubleRec n 10 0

hasDoubleRec :: Int -> Int -> Int -> Bool
hasDoubleRec 0 _ c = c == 2
hasDoubleRec n last count
        | d /= last && count == 2 = True
        | d /= last               = hasDoubleRec (n `div` 10) d 1
        | otherwise               = hasDoubleRec (n `div` 10) d (count + 1)
        where d = n `mod` 10

isIncreasing :: Int -> Bool
isIncreasing n = isIncreasingRec n 10

isIncreasingRec :: Int -> Int -> Bool
isIncreasingRec 0 _    = True
isIncreasingRec n last = (d <= last) && isIncreasingRec (n `div` 10) d
                         where d = n `mod` 10

sol2 :: String -> Int
sol2 s = length [ x | x <- [b..e], isValid2 x ]
    where nums   = map U.readInt $ U.splitOn '-' s
          (b, e) = (head nums, nums!!1)

main = do
    [filename] <- getArgs
    handle     <- openFile filename ReadMode
    contents   <- hGetContents handle
    print $ sol1 contents
    print $ sol2 contents
