import Data.Set (Set, intersection, fromList, take, toList)
import Data.Char (ord, isLowerCase)

commonItem :: String -> Char
commonItem backpack = head $ toList $ intersection (firstCompartment backpack) (secondCompartment backpack)

firstCompartment :: String -> Set Char
firstCompartment s = fromList $ Prelude.take (length s `div` 2) s

secondCompartment :: String -> Set Char
secondCompartment s = fromList $ drop (length s `div` 2) s

priority :: Char -> Int
priority x
  | isLowerCase x = ord x - ord 'a' + 1
  | otherwise     = ord x - ord 'A' + 27

main :: IO()
main =
  do
    contents <- readFile "../inputs/day03/input"
    let backpacks = lines contents
    print $ sum $ map (priority . commonItem) backpacks