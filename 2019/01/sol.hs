module Main where

import System.IO
import Debug.Trace
import Utility as U


sol1 :: String -> Int
sol1 s = sum $ map strToFuel (U.splitLines s)
            where strToFuel x = getFuel $ U.readInt x

getFuel :: Int -> Int
getFuel mass = mass `div` 3 - 2

sol2 :: String -> Int
sol2 s = sum $ map strToFullFuel (U.splitLines s)
            where strToFullFuel x = getFullFuel $ U.readInt x

fuelFuel :: Int -> Int
fuelFuel f = fuelFuelRec (getFuel f) 0

fuelFuelRec :: Int -> Int -> Int
fuelFuelRec curr acc = 
    if curr <= 0 then acc
    else fuelFuelRec newCurr (acc + curr)
        where newCurr = getFuel curr

getFullFuel :: Int -> Int
getFullFuel mass = fuel + fuelFuel fuel
    where fuel = getFuel mass

main :: IO ()
main = do
        handle <- openFile "input0.txt" ReadMode
        contents <- hGetContents handle
        -- print $ getFullFuel 1969
        -- print $ getFullFuel 100756
        print $ sol1 contents
        print $ sol2 contents