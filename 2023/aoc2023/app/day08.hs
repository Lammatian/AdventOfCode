import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Map as M (Map, fromList, lookup, keys)
import Data.CircularList as C (CList, focus, rotR, fromList)

data Direction = L | R deriving (Show, Read)
type Location = String
type Instruction = (Location, (Location, Location))
type Instructions = Map Location (Location, Location)

parseInstruction :: String -> Instruction
parseInstruction s = (f, (l, r))
  where
    [f, lr] = splitOn " = (" s
    l = head $ splitOn ", " lr
    r = init $ last $ splitOn ", " lr

parseInstructions :: [String] -> Instructions
parseInstructions = M.fromList . map parseInstruction

turn :: (Location, Location) -> Direction -> Location
turn (l, _) L = l
turn (_, r) R = r

nextLocation :: Instructions -> Location -> Direction -> Location
nextLocation is l d = case M.lookup l is of
  Just x -> turn x d
  Nothing -> error "Location not found in the map: " ++ l

isStrictEnd :: Location -> Bool
isStrictEnd "ZZZ" = True
isStrictEnd _ = False

isRelaxedEnd :: Location -> Bool
isRelaxedEnd = (== 'Z') . last

followInstructions :: CList Direction -> Location -> (Location -> Bool) -> Instructions -> Int
followInstructions ds loc isEnd is
  | isEnd loc = 0
  | otherwise  = 1 + followInstructions (rotR ds) nextLoc isEnd is
  where
    d = case focus ds of
      Just x -> x
      Nothing -> error "This should never happen :("
    nextLoc = nextLocation is loc d

nlcm :: [Integer] -> Integer
nlcm = foldl lcm 1

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day08/input.txt"
    let directions = C.fromList $ (map (\x -> (read :: String -> Direction) [x]) . head . lines) contents
    let instructions = parseInstructions $ drop 2 $ lines contents
    print $ followInstructions directions "AAA" isStrictEnd instructions
    let ghostStarts = filter ((== 'A') . last) $ keys instructions
    let cycleLengths = map (\start -> followInstructions directions start isRelaxedEnd instructions) ghostStarts
    print $ nlcm $ map toInteger cycleLengths