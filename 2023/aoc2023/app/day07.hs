{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import System.Environment (getArgs)
import Data.Map (fromListWith, elems)
import Data.List (sortOn, sort)
import Data.Char (isDigit, digitToInt)

type Cards = [Int]
type Counts = [Int]
type Bet = Int
data Hand = Hand {
  unCards :: Cards,
  unCounts :: Counts,
  unBet :: Bet
} deriving (Show, Eq)

instance Ord Hand where
  (Hand cards1 counts1 _) `compare` (Hand cards2 counts2 _) = [counts1, cards1] `compare` [counts2, cards2]

parseCard :: Char -> Int -> Int
parseCard c jval
  | isDigit c = digitToInt c
  | c == 'T'  = 10
  | c == 'J'  = jval
  | c == 'Q'  = 12
  | c == 'K'  = 13
  | c == 'A'  = 14
  | otherwise = error $ "Unexpected value for a card " ++ [c]

parseCards :: String -> Int -> Cards
parseCards s jval = case length s of
  5 -> map (`parseCard` jval) s
  _ -> error "Unexpected number of cards for a hand"

getCardCounts :: Cards -> Counts
getCardCounts cs = sortOn (\x -> -x) $ elems $ fromListWith (+) $ map (\c -> (c, 1)) cs

parseHand :: String -> Int -> Hand
parseHand s jval = Hand cards cardCounts ((read :: String -> Int) betStr)
  where
    [cardStr, betStr] = words s
    cards = parseCards cardStr jval
    cardCounts
      | jval == 11 = getCardCounts cards
      | otherwise  = getCardCountsWithJoker cards

getCardCountsWithJoker :: Cards -> Counts
getCardCountsWithJoker cs = if jokerCount == 5 then [5] else (head nonJokerCounts + jokerCount) : tail nonJokerCounts
  where
    nonJokerCards = filter (/=0) cs
    jokerCount = length $ filter (==0) cs
    nonJokerCounts = sortOn (\x -> -x) $ elems $ fromListWith (+) $ zip nonJokerCards (replicate 5 1)

getTotalWinnings :: [Hand] -> Int
getTotalWinnings hs = foldl (\acc (h, i) -> acc + i * unBet h) 0 (zip hs [1..])

main :: IO()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args  else "inputs/day07/input.txt"
    let sortedHands = (sort . map (`parseHand` 11) . lines) contents
    print $ getTotalWinnings sortedHands
    let sortedJokerHands = (sort . map (`parseHand` 0) . lines) contents
    print $ getTotalWinnings sortedJokerHands