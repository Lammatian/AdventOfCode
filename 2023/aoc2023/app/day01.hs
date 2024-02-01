import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)
import Data.List.NonEmpty as NE (fromList, NonEmpty, head, last)
import Util (readInput)

numDigits :: String -> [Int]
numDigits = map digitToInt . filter isDigit

allDigits :: String -> [Int]
allDigits [] = []
allDigits s@(x:xs)
  | isDigit x               = digitToInt x : allDigits xs
  | "one"   `isPrefixOf` s  = 1 : allDigits xs
  | "two"   `isPrefixOf` s  = 2 : allDigits xs
  | "three" `isPrefixOf` s  = 3 : allDigits xs
  | "four"  `isPrefixOf` s  = 4 : allDigits xs
  | "five"  `isPrefixOf` s  = 5 : allDigits xs
  | "six"   `isPrefixOf` s  = 6 : allDigits xs
  | "seven" `isPrefixOf` s  = 7 : allDigits xs
  | "eight" `isPrefixOf` s  = 8 : allDigits xs
  | "nine"  `isPrefixOf` s  = 9 : allDigits xs
  | otherwise               = allDigits xs

firstLast :: NonEmpty Int -> Int
firstLast x = 10 * NE.head x + NE.last x

convertArt :: (String -> [Int]) -> String -> Int
convertArt digitExtractor = firstLast . NE.fromList . digitExtractor

main :: IO()
main =
  do
    content <- lines <$> readInput 1
    print $ sum $ map (convertArt numDigits) content
    print $ sum $ map (convertArt allDigits) content
