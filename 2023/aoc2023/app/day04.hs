import Data.Set (Set, fromList, intersection, size)
import Data.List.Split (splitOn)
import Control.Exception (throwIO)

data Card = Card !(Set Int) !(Set Int)

parseListOnCard :: String -> Set Int
parseListOnCard = fromList . map (read :: String -> Int) . words

parseCardString :: String -> IO Card
parseCardString s = case length splitOnBar of
  2 -> pure $ Card (parseListOnCard (head splitOnBar)) (parseListOnCard (last splitOnBar))
  _ -> throwIO $ userError "unexpected number of '|' in string"
  where
    splitOnBar = (splitOn " | " . last . splitOn ": ") s

parseInput :: String -> IO [Card]
parseInput s =
  do
    let ls = lines s
    mapM parseCardString ls

scratchcardPoints :: Card -> Int
scratchcardPoints (Card winning owned) = if matches > 0 then 2 ^ (matches - 1) else 0
  where
    matches = size (intersection winning owned)

-- Given a card and number of copies, return how many copies of subsequent scratchcards are to be added
scratchcardsWon :: Card -> Int -> [Int]
scratchcardsWon (Card winning owned) = replicate $ size $ intersection winning owned

-- Given current number of copies and new copies, return the updated list
-- In other words, sum the two lists and add remaining elements of the first list at the end
updateNumberOfCopies :: [Int] -> [Int] -> [Int]
updateNumberOfCopies (a:as) (b:bs) = a + b : updateNumberOfCopies as bs
updateNumberOfCopies as _ = as

totalScratchcardCount :: [Card] -> [Int] -> Int -> Int
totalScratchcardCount (card:cards) (currentCardCount:copies) total = totalScratchcardCount cards updatedNumberOfCopies (total + currentCardCount)
  where
    updatedNumberOfCopies = updateNumberOfCopies copies $ scratchcardsWon card currentCardCount
totalScratchcardCount _ _ t = t

main :: IO ()
main =
  do
    contents <- readFile "inputs/day04/input.txt"
    cards <- parseInput contents
    print $ sum $ map scratchcardPoints cards
    print $ totalScratchcardCount cards (replicate (length cards) 1) 0
    -- Future reference: if I ignore nice typing, I can also do this beauty XD
    -- let cards = map (map (fromList . map (read :: String -> Int) . words) . splitOn " | " . last . splitOn ": ") $ lines contents
