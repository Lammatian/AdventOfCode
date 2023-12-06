import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intercalate)

recordBeaterCount :: Int -> Int -> Int -> Int
recordBeaterCount time recordDistance windUp = if distanceTravelled > recordDistance
  -- There are (t + 1) options in total, out of which first w and last w won't work
  then (time + 1) - 2 * windUp
  else recordBeaterCount time recordDistance (windUp + 1)
  where
    distanceTravelled = (time - windUp) * windUp

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args  else "inputs/day06/input.txt"
    let [timesStr, distancesStr] = lines contents
    let times = (map (read :: String -> Int) . words . last . splitOn ": ") timesStr
    let recordDistances = (map (read :: String -> Int) . words . last . splitOn ": ") distancesStr
    print $ product $ [recordBeaterCount t d 0 | (t, d) <- zip times recordDistances]
    let actualTime = (read :: String -> Int) $ intercalate "" $ (words . last . splitOn ": ") timesStr
    let actualRecordDistance = (read :: String -> Int) $ intercalate "" $ (words . last . splitOn ": ") distancesStr
    print $ recordBeaterCount actualTime actualRecordDistance 0

