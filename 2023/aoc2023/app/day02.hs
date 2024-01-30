import Data.List.Split (splitOn)
import Data.Map (fromList, findWithDefault)
import Data.Tuple (swap)
import Lib (bisect, bisectOn)

data Game = Game {
  gameId :: Int,
  diceReveals :: [CubeReveal]
} deriving (Show)
data CubeReveal = CubeReveal {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Show)

-- format: "X [red|green|blue], Y [red|green|blue], Z [red|green|blue]"
instance Read CubeReveal where
  readsPrec _ input =
    let valueMap = fromList $ map (swap . bisect) $ splitOn ", " input
        r = read (findWithDefault "0" "red" valueMap) :: Int
        g = read (findWithDefault "0" "green" valueMap) :: Int
        b = read (findWithDefault "0" "blue" valueMap) :: Int
    -- The second argument here is the remainder of the string after successful parsing
    in [(CubeReveal r g b, "")]

-- format: "Game G: X red, Y green, Z blue; ..."
instance Read Game where
  readsPrec _ input =
    let (game, rest) = bisectOn ": " input
        gId = read (last $ splitOn " " game) :: Int
        revealList = splitOn "; " rest
        reveals = map (read :: String -> CubeReveal) revealList
    -- The second argument here is the remainder of the string after successful parsing
    in [(Game gId reveals, "")]

isCubeRevealPossible :: CubeReveal -> Bool
isCubeRevealPossible dr = red dr <= 12 && green dr <= 13 && blue dr <= 14

isGamePossible :: Game -> Bool
isGamePossible g = all isCubeRevealPossible (diceReveals g)

minimalCubeRevealPossible :: Game -> CubeReveal
minimalCubeRevealPossible g = CubeReveal mr mg mb
  where
    mr = maximum $ map red (diceReveals g)
    mg = maximum $ map green (diceReveals g)
    mb = maximum $ map blue (diceReveals g)

cubePower :: CubeReveal -> Int
cubePower dr = red dr * green dr * blue dr

main :: IO()
main =
  do
    games <- map (read :: String -> Game) . lines <$> readFile "inputs/day02/input.txt"
    let possibleGames = filter isGamePossible games
    print $ sum $ map gameId possibleGames
    print $ sum $ map (cubePower . minimalCubeRevealPossible) games