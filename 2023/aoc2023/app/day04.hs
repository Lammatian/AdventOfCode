import Data.Set (Set, fromList, intersection, size)
import Data.List.Split (splitOn)

scratchcardPoints :: Set Int -> Set Int -> Int
scratchcardPoints winning owned = if matches > 0 then 2 ^ (matches - 1) else 0
  where matches = size (intersection winning owned)

main :: IO()
main =
  do
    contents <- readFile "inputs/day04/input.txt"
    -- XD
    let cards = map (map (fromList . map (read :: String -> Int) . words) . splitOn " | " . last . splitOn ": ") $ lines contents
    print $ sum $ map (\[x, y] -> scratchcardPoints x y) cards
