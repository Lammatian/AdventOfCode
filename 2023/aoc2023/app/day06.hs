import Data.List.Split (splitOn)
import Util (readInput)

parseInputLine :: String -> [Int]
parseInputLine = map read . words . last . splitOn ": "

parseActualInputLine :: String -> Int
parseActualInputLine = (read :: String -> Int) . filter (/=' ') . last . splitOn ": "

recordBeaterCount :: Int -> Int -> Int
recordBeaterCount time recordDistance = recordBeaterCountRec time recordDistance 0

recordBeaterCountRec :: Int -> Int -> Int -> Int
recordBeaterCountRec time recordDistance windUp = if distanceTravelled > recordDistance
  -- There are (t + 1) options in total, out of which first w and last w won't work
  then (time + 1) - 2 * windUp
  else recordBeaterCountRec time recordDistance (windUp + 1)
  where
    distanceTravelled = (time - windUp) * windUp

main :: IO ()
main =
  do
    -- I don't like enforcing this having two lines with an error, I'll leave it as is
    [timesStr, distancesStr] <- lines <$> readInput 6
    let times = parseInputLine timesStr
    let recordDistances = parseInputLine distancesStr
    print $ product $ zipWith recordBeaterCount times recordDistances
    let actualTime = parseActualInputLine timesStr
    let actualRecordDistance = parseActualInputLine distancesStr
    print $ recordBeaterCount actualTime actualRecordDistance
