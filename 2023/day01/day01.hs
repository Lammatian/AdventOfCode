import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)
import Data.List.NonEmpty as NE (fromList, NonEmpty, head, last)

onlyDigits :: String -> [Int]
onlyDigits = map digitToInt . filter isDigit

onlyDigitsSpelled :: String -> [Int]
onlyDigitsSpelled [] = []
onlyDigitsSpelled (x:xs)
  | isDigit x                   = digitToInt x : onlyDigitsSpelled xs
  | isPrefixOf "one" (x:xs)     = 1 : onlyDigitsSpelled xs
  | isPrefixOf "two" (x:xs)     = 2 : onlyDigitsSpelled xs
  | isPrefixOf "three" (x:xs)   = 3 : onlyDigitsSpelled xs
  | isPrefixOf "four" (x:xs)    = 4 : onlyDigitsSpelled xs
  | isPrefixOf "five" (x:xs)    = 5 : onlyDigitsSpelled xs
  | isPrefixOf "six" (x:xs)     = 6 : onlyDigitsSpelled xs
  | isPrefixOf "seven" (x:xs)   = 7 : onlyDigitsSpelled xs
  | isPrefixOf "eight" (x:xs)   = 8 : onlyDigitsSpelled xs
  | isPrefixOf "nine" (x:xs)    = 9 : onlyDigitsSpelled xs
  | otherwise                   = onlyDigitsSpelled xs

firstLast :: NonEmpty Int -> Int
firstLast x = 10 * NE.head x + NE.last x

convertArt :: (String -> [Int]) -> String -> Int
convertArt digitExtractor = firstLast . NE.fromList . digitExtractor

main :: IO()
main = 
  do
    content <- readFile ("input.txt")
    let contentLines = lines content
    print $ sum $ map (convertArt onlyDigits) contentLines
    print $ sum $ map (convertArt onlyDigitsSpelled) contentLines