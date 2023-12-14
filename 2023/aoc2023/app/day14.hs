{-# LANGUAGE NumericUnderscores #-}
import System.Environment (getArgs)
import Data.Map as M (Map, lookup, insert, empty)

type Column = String
type Row = String
type Board = [String]

column :: Int -> Board -> Column
column s = map (!! s)

rockfall :: Column -> Int -> Int -> [Int]
rockfall [] _ _ = []
rockfall (x:xs) lastHash curr
  | x == '.'  = rockfall xs lastHash (curr + 1)
  | x == '#'  = rockfall xs curr (curr + 1)
  | otherwise = (lastHash + 1) : rockfall xs (lastHash + 1) (curr + 1)

rockfallValue :: [Int] -> Int -> Int
rockfallValue xs r = sum $ map (r -) xs

rockfallToRow :: Column -> [Int] -> Row
rockfallToRow col rf = reverse $ go col rf 0
  where
    go [] _ _ = []
    go (x:xs) [] i
      | x == 'O'  = '.' : go xs [] (i + 1)
      | otherwise = x : go xs [] (i + 1)
    go (x:xs) (r:rs) i
      | r == i    = 'O' : go xs rs (i + 1)
      | x == 'O'  = '.' : go xs (r:rs) (i + 1)
      | otherwise = x : go xs (r:rs) (i + 1)

rockfallAndRotate :: Board -> Board
rockfallAndRotate b = go (map (`column` b) [0..length (head b) - 1])
  where
    go [] = []
    go (c:cs) = rockfallToRow c (rockfall c (-1) 0) : go cs

spinCycle :: Board -> Board
spinCycle b = iterate rockfallAndRotate b !! 4

findRepeat :: Board -> Map Board Int -> Int -> (Int, Int)
findRepeat b m c = case b `M.lookup` m of
  Just x  -> (c, c - x)
  Nothing -> findRepeat (spinCycle b) (M.insert b c m) (c + 1)

columnValue :: Column -> Int -> Int
columnValue [] _ = 0
columnValue ('O':cs) r = r + columnValue cs (r - 1)
columnValue (_:cs) r = columnValue cs (r - 1)

boardValue :: Board -> Int
boardValue b = sum $ map (\c -> columnValue c (length b)) columns
  where
    columns = map (`column` b) [0..length (head b) - 1]

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day14/input.txt"
    let board = lines contents
    let rockfalls = map ((\c -> rockfall c (-1) 0) . (`column` board)) [0..length (head board) - 1]
    print $ sum $ map (\r -> rockfallValue r (length board)) rockfalls
    let (repeatStart, repeatCycleLength) = findRepeat board M.empty 0
    let iterationsNeeded = repeatStart + ((1_000_000_000 - repeatStart) `mod` repeatCycleLength)
    print $ boardValue $ iterate spinCycle board !! iterationsNeeded