import Data.Char (isDigit, digitToInt)
import Data.Map as DM (Map, empty, insertWith, filter, map)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

type Row = String
type TotalSum = Int
type CurrentNumber = Int
type IsSymbolAdjacent = Bool

isSymbol :: Char -> Bool
isSymbol c | isDigit c = False
           | c == '.'  = False
           | otherwise = True

-- Assumes rows are wrapped in . at the front and the back
rowPartNumbers :: Row -> Row -> Row -> TotalSum -> CurrentNumber -> IsSymbolAdjacent -> Int
rowPartNumbers (a:as) (b1:b2:b3:bs) (c:cs) tsum curr adj
  -- construct number
  | isDigit b2 = rowPartNumbers as (b2:b3:bs) cs tsum updatedNumber (adj || symbolAdjacent)
  -- number constructed: add to total sum if adjacent to a symbol and reset
  | curr > 0 = rowPartNumbers as (b2:b3:bs) cs (tsum + if adj then curr else 0) 0 False
  -- not a digit and no sum currently, continue
  | otherwise = rowPartNumbers as (b2:b3:bs) cs tsum 0 False
  where
    symbolInRow = any isSymbol
    symbolAdjacent = symbolInRow (take 3 (a:as)) || symbolInRow [b1, b2, b3] || symbolInRow (take 3 (c:cs))
    updatedNumber = 10 * curr + digitToInt b2
-- Reached border and a number still to be added
rowPartNumbers _ [_, _] _ tsum curr True = tsum + curr
rowPartNumbers _ _ _ tsum _ _ = tsum

findGear :: Int -> Int -> Row -> Row -> Row -> Maybe (Int, Int)
findGear r c r1 r2 r3 = case Prelude.map (elemIndex '*') [r1, r2, r3] of
  [Just x, Nothing, Nothing] -> Just (r - 1, c + x - 1)
  [Nothing, Just x, Nothing] -> Just (r, c + x - 1)
  [Nothing, Nothing, Just x] -> Just (r + 1, c + x - 1)
  _nothingFound -> Nothing

updateGear :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
updateGear (Just x) _ = Just x
updateGear Nothing (Just x) = Just x
updateGear _ _ = Nothing

-- Let's assume each number is adjacent only to one gear and hope for the best
-- Returns the map with gear (x, y) coordinates as key and list of parts as value
partsNextToGears :: Int -> Int -> Row -> Row -> Row -> CurrentNumber -> Maybe (Int, Int) -> Map (Int, Int) [Int] -> Map (Int, Int) [Int]
partsNextToGears row col (a:as) (b1:b2:b3:bs) (c:cs) curr gear m
  | isDigit b2 = partsNextToGears row nextCol as (b2:b3:bs) cs (10 * curr + digitToInt b2) (updateGear gear newGear) m
  | curr > 0 = partsNextToGears row nextCol as (b2:b3:bs) cs 0 Nothing updateGearMap
  | otherwise = partsNextToGears row nextCol as (b2:b3:bs) cs 0 Nothing m
  where
    nextCol = col + 1
    newGear = findGear row col (take 3 (a:as)) [b1, b2, b3] (take 3 (c:cs))
    updateGearMap = if isJust gear then insertWith (++) (fromJust gear) [curr] m else m
partsNextToGears _ _ _ _ _ curr (Just (x, y)) m = insertWith (++) (x, y) [curr] m
partsNextToGears _ _ _ _ _ _ _ m = m

rowGearRatio :: Map (Int, Int) [Int] -> Int
rowGearRatio m = sum $ DM.map (\[x, y] -> x * y) $ DM.filter (\x -> length x == 2) m

main :: IO()
main =
  do
    -- make a border of '.'
    -- iterate three rows at once, three characters at once
    -- keep track of total sum for the row, current number, if adjacent to symbol
    contents <- readFile "inputs/day03/input.txt"
    let rows = Prelude.map (\l -> "." ++ l ++ ".") $ lines contents
    let rowLength = length $ head rows
    let wrappedRows = [concat (replicate rowLength ".")] ++ rows ++ [concat (replicate rowLength ".")]
    let zippedWrappedRows = zip (zip wrappedRows (tail wrappedRows)) (drop 2 wrappedRows)
    print $ sum $ Prelude.map (\((a, b), c) -> rowPartNumbers a b c 0 0 False) zippedWrappedRows
    print $ rowGearRatio $ foldr (\(((a, b), c), r) m -> partsNextToGears r 1 a b c 0 Nothing m) empty $ zip zippedWrappedRows [1..]