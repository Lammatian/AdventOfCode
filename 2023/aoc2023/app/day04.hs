import Data.Set (Set, fromList, intersection, size)
import Util (readInput, bisectOn)

type Card = (Set Int, Set Int)

parseListOnCard :: String -> Set Int
parseListOnCard = fromList . map (read :: String -> Int) . words

parseCardString :: String -> Card
parseCardString s = (parseListOnCard c1, parseListOnCard c2)
  where
    (c1, c2) = (bisectOn " | " . snd . bisectOn ": ") s

matches :: Card -> Int
matches (winning, owned) = size $ intersection winning owned

scratchcardPoints :: Card -> Int
scratchcardPoints c = if m > 0 then 2 ^ (m - 1) else 0
  where
    m = matches c

-- Given a card and number of copies, return how many copies of subsequent scratchcards are to be added
-- E.g. if card has 3 matches and there are 4 copies of it, returns [4,4,4]
scratchcardsWon :: Card -> Int -> [Int]
scratchcardsWon = replicate . matches

-- Given current scratchcard copies and new copies, update overall copies
updateCopies :: [Int] -> [Int] -> [Int]
updateCopies xs ys = zipWith (+) xs ys ++ drop (length ys) xs

totalScratchcardCount :: [Card] -> [Int] -> Int
totalScratchcardCount [] _ = 0
totalScratchcardCount _ [] = 0
totalScratchcardCount (card:cards) (curCopies:copies) = curCopies + totalScratchcardCount cards updatedCopies
  where
    updatedCopies = updateCopies copies $ scratchcardsWon card curCopies

main :: IO ()
main =
  do
    cards <- map parseCardString . lines <$> readInput 4
    print $ sum $ map scratchcardPoints cards
    print $ totalScratchcardCount cards (replicate (length cards) 1)
