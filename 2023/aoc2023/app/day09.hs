import System.Environment (getArgs)

extrapolate :: [Integer] -> Integer
extrapolate is
  | all (== 0) diffs = last is
  | otherwise        = last is + extrapolate diffs
  where
    diffs = zipWith (-) (tail is) is

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day09/input.txt"
    let series =  map (map (read :: String -> Integer) . words) $ lines contents
    print $ sum $ map extrapolate series
    print $ sum $ map (extrapolate . reverse) series