{-# LANGUAGE TupleSections #-}
import System.Environment (getArgs)

data Point = P {
  row :: Int,
  col :: Int
} deriving (Show)

printGrid :: [String] -> IO ()
printGrid [] = print ""
printGrid [x] = print x
printGrid (s:ss) =
  do
    print s
    printGrid ss

getExpandedRows :: [String] -> Int -> [Int]
getExpandedRows [] _ = []
getExpandedRows (s:ss) r
  | all (== '.') s = r : getExpandedRows ss (r + 1)
  | otherwise      = getExpandedRows ss (r + 1)

getExpandedColumns :: [String] -> Int -> [Int]
getExpandedColumns [] _ = []
getExpandedColumns ss c
  | all null ss              = []
  | all ((== '.') . head) ss = c : getExpandedColumns (map tail ss) (c + 1)
  | otherwise                = getExpandedColumns (map tail ss) (c + 1)

getGalaxiesInRow :: String -> Int -> [Int]
getGalaxiesInRow [] _ = []
getGalaxiesInRow (s:ss) c
  | s == '#'  = c : getGalaxiesInRow ss (c + 1)
  | otherwise = getGalaxiesInRow ss (c + 1)

getGalaxies :: [String] -> Int -> [Point]
getGalaxies [] _ = []
getGalaxies (s:ss) r = map (P r) galaxiesInRow ++ getGalaxies ss (r + 1)
  where
    galaxiesInRow = getGalaxiesInRow s 0

pairs :: [Point] -> [(Point, Point)]
pairs [] = []
pairs (p:ps) = map (p,) ps ++ pairs ps

rawDistance :: Point -> Point -> Integer
rawDistance (P r1 c1) (P r2 c2) = toInteger $ abs (r1 - r2) + abs (c1 - c2)

countExpandedRows :: [Int] -> Point -> Point -> Int
countExpandedRows es (P r1 _) (P r2 _) = length $ filter (\r -> r > minR && r < maxR) es
  where
    minR = min r1 r2
    maxR = max r1 r2

countExpandedCols :: [Int] -> Point -> Point -> Int
countExpandedCols es (P _ c1) (P _ c2) = length $ filter (\c -> c > minC && c < maxC) es
  where
    minC = min c1 c2
    maxC = max c1 c2

distance :: Point -> Point -> [Int] -> [Int] -> Integer -> Integer
distance p1 p2 ers ecs expSize = rawDistance p1 p2 + expSize * (expandedRows + expandedCols)
  where
    expandedRows = toInteger $ countExpandedRows ers p1 p2
    expandedCols = toInteger $ countExpandedCols ecs p1 p2

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day11/input.txt"
    let image = lines contents
    let galaxies = getGalaxies image 0
    let galaxyPairs = pairs galaxies
    let expandedRows = getExpandedRows image 0
    let expandedCols = getExpandedColumns image 0
    print $ sum $ map (\(p1, p2) -> distance p1 p2 expandedRows expandedCols 1) galaxyPairs
    print $ sum $ map (\(p1, p2) -> distance p1 p2 expandedRows expandedCols 999999) galaxyPairs