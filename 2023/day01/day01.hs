import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)
import Data.List.NonEmpty as NE (fromList, NonEmpty, head, last)

onlyDigits :: String -> [Int]
onlyDigits = map digitToInt . filter isDigit

onlyDigitsSpelled :: String -> [Int]
onlyDigitsSpelled [] = []
onlyDigitsSpelled (x:xs)
  | isDigit x               = digitToInt x : onlyDigitsSpelled xs
  | "one" `isPrefixOf` s    = 1 : onlyDigitsSpelled xs
  | "two" `isPrefixOf` s    = 2 : onlyDigitsSpelled xs
  | "three" `isPrefixOf` s  = 3 : onlyDigitsSpelled xs
  | "four" `isPrefixOf` s   = 4 : onlyDigitsSpelled xs
  | "five" `isPrefixOf` s   = 5 : onlyDigitsSpelled xs
  | "six" `isPrefixOf` s    = 6 : onlyDigitsSpelled xs
  | "seven" `isPrefixOf` s  = 7 : onlyDigitsSpelled xs
  | "eight" `isPrefixOf` s  = 8 : onlyDigitsSpelled xs
  | "nine" `isPrefixOf` s   = 9 : onlyDigitsSpelled xs
  | otherwise               = onlyDigitsSpelled xs
  where s = x:xs

firstLast :: NonEmpty Int -> Int
firstLast x = 10 * NE.head x + NE.last x

convertArt :: (String -> [Int]) -> String -> Int
convertArt digitExtractor = firstLast . NE.fromList . digitExtractor

main :: IO()
main = 
  do
    content <- readFile "input.txt"
    let contentLines = lines content
    print $ sum $ map (convertArt onlyDigits) contentLines
    print $ sum $ map (convertArt onlyDigitsSpelled) contentLines