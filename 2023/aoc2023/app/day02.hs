import Data.List.Split (splitOn)
import Data.List (find)

data Game = Game {
  gameId :: Int,
  diceReveals :: [DiceReveal]
} deriving (Show)
data DiceReveal = DiceReveal {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Show)

getValue :: [[String]] -> String -> Int
getValue xs key = case find (\[_, c] -> c == key) xs of
                    Just [x, _] -> read x :: Int
                    Nothing -> 0

-- format: "X [red|green|blue], Y [red|green|blue], Z [red|green|blue]"
instance Read DiceReveal where
  readsPrec _ input =
    let values = map words $ splitOn ", " input
        r = getValue values "red"
        g = getValue values "green"
        b = getValue values "blue"
    -- No idea why, but if I return input as the second argument, it crashes lol
    in [(DiceReveal r g b, "")]

-- format: "Game G: X red, Y green, Z blue; ..."
instance Read Game where
  readsPrec _ input =
    let [game, reveals] = splitOn ": " input
        gId = read (last $ splitOn " " game) :: Int
        drs = splitOn "; " reveals
        pr = map (read :: String -> DiceReveal) drs
    -- No idea why, but if I return input as the second argument, it crashes lol
    in [(Game gId pr, "")]

isDiceRevealPossible :: DiceReveal -> Bool
isDiceRevealPossible dr = red dr <= 12 && green dr <= 13 && blue dr <= 14

isGamePossible :: Game -> Bool
isGamePossible g = all isDiceRevealPossible (diceReveals g)

minimalDiceRevealPossible :: Game -> DiceReveal
minimalDiceRevealPossible g = DiceReveal mr mg mb
  where
    mr = maximum $ map red (diceReveals g)
    mg = maximum $ map green (diceReveals g)
    mb = maximum $ map blue (diceReveals g)

cubePower :: DiceReveal -> Int
cubePower dr = red dr * green dr * blue dr

main :: IO()
main =
  do
    contents <- readFile "inputs/day02/input.txt"
    let games = map (read :: String -> Game) $ lines contents
    let possibleGames = filter isGamePossible games
    print $ sum $ map gameId possibleGames
    print $ sum $ map (cubePower . minimalDiceRevealPossible) games