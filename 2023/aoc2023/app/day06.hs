import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intercalate)

distanceTravelled :: Int -> Int -> Int
distanceTravelled time holdTime = (time - holdTime) * holdTime

possibleDistances :: Int -> [Int]
possibleDistances time = map (distanceTravelled time) [0..time]

recordBeaterCount :: Int -> Int -> Int
recordBeaterCount time recordDistance = length $ filter (> recordDistance) (possibleDistances time)

distanceTravelled2 :: Integer -> Integer -> Integer
distanceTravelled2 time holdTime = (time - holdTime) * holdTime

recordBeaterCountRec :: Integer -> Integer -> Integer -> Integer
recordBeaterCountRec time recordDistance windUp 
  | time == windUp = 0
  | otherwise = (if distanceTravelled2 time windUp > recordDistance then 1 else 0) + recordBeaterCountRec time recordDistance (windUp + 1)

-- test :: Integer -> Integer -> Integer -> Integer
-- test t d w = if (t - w) * w > d then t - 2 * (w - 1) else test t d (w + 1)

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args  else "inputs/day06/input.txt"
    let [timesStr, distancesStr] = lines contents
    let times = (map (read :: String -> Int) . words . last . splitOn ": ") timesStr
    let recordDistances = (map (read :: String -> Int) . words . last . splitOn ": ") distancesStr
    print $ product $ [recordBeaterCount t d | (t, d) <- zip times recordDistances]
    let actualTime = (read :: String -> Integer) $ intercalate "" $ (words . last . splitOn ": ") timesStr
    let actualRecordDistance = (read :: String -> Integer) $ intercalate "" $ (words . last . splitOn ": ") distancesStr
    print $ recordBeaterCountRec actualTime actualRecordDistance 0
    -- print $ test actualTime actualRecordDistance 0

