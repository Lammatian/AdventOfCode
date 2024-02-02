import Data.List.Split (splitOn)
import Data.Map as M (Map, fromList, lookup, keys)
import Data.CircularList as C (CList, focus, rotR, fromList)
import Util (readInput, bisectOn)

data Direction = L | R deriving (Show, Read)
type Location = String
type Node = (Location, (Location, Location))
type Graph = Map Location (Location, Location)

parseNode :: String -> Node
parseNode s = (f, (l, r))
  where
    (f, lr) = bisectOn " = (" s
    l = head $ splitOn ", " lr
    r = init $ last $ splitOn ", " lr

parseGraph :: [String] -> Graph
parseGraph = M.fromList . map parseNode

turn :: (Location, Location) -> Direction -> Location
turn (l, _) L = l
turn (_, r) R = r

nextLocation :: Graph -> Location -> Direction -> Location
nextLocation is l d = case M.lookup l is of
  Just x  -> turn x d
  Nothing -> error "Location not found in the map: " ++ l

isStrictEnd :: Location -> Bool
isStrictEnd "ZZZ" = True
isStrictEnd _ = False

isRelaxedEnd :: Location -> Bool
isRelaxedEnd = (== 'Z') . last

followDirections :: Graph -> CList Direction -> Location -> (Location -> Bool) -> Int
followDirections is ds loc isEnd
  | isEnd loc = 0
  | otherwise = 1 + followDirections is (rotR ds) nextLoc isEnd
  where
    d = case focus ds of
      Just x  -> x
      Nothing -> error "This should never happen :("
    nextLoc = nextLocation is loc d

nlcm :: [Integer] -> Integer
nlcm = foldl lcm 1

main :: IO ()
main =
  do
    contents <- lines <$> readInput 8
    let directions = C.fromList $ (map (\x -> (read :: String -> Direction) [x]) . head) contents
    let graph = parseGraph $ drop 2 contents
    print $ followDirections graph directions "AAA" isStrictEnd
    let ghostStarts = filter ((== 'A') . last) $ keys graph
    let cycleLengths = map (\start -> followDirections graph directions start isRelaxedEnd) ghostStarts
    print $ nlcm $ map toInteger cycleLengths