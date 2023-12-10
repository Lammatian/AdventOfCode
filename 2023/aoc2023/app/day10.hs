import System.Environment (getArgs)
import Data.Vector as V (Vector, fromList, (!), (//), length, head, tail)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set as S (Set, fromList, difference, toList)

type Grid = Vector (Vector Tile)
data Direction = N | E | S | W deriving (Show, Eq)
data Tile = NS | EW | NE | NW | SW | SE | Invalid | Wall | Insider | Visited deriving (Eq)
data Point = Point {
  row :: Int,
  col :: Int
} deriving (Show, Eq, Ord)
-- TODO: This is so unnecessary lol
data Position = Pos {
  grid :: Grid,
  point :: Point
} deriving (Show)

instance Show Tile where
  show NS = "|"
  show EW = "-"
  show NE = "└"
  show NW = "┘"
  show SW = "┐"
  show SE = "┌"
  show Invalid = "."
  show Wall = "#"
  show Insider = "I"
  show Visited = "V"

-- This is awful lol
printGrid :: Grid -> IO ()
printGrid g = if V.length g == 0 then print "" else
  do
    print (V.head g)
    printGrid (V.tail g)

toTile :: Char -> Tile
toTile c | c == '|'  = NS
         | c == '-'  = EW
         | c == 'L'  = NE
         | c == 'J'  = NW
         | c == '7'  = SW
         | c == 'F'  = SE
         -- This is hardcoded lol
         | c == 'S'  = NW
         -- Samples
        --  | c == 'S'  = SE
         | otherwise = Invalid

at :: Grid -> Point -> Tile
g `at` Point r c = (g ! r) ! c

currTile :: Position -> Tile
currTile (Pos g p) = at g p

move :: Position -> Direction -> Position
-- Why is this non-exhaustive?
move (Pos g (Point r c)) dir
  | dir == N = Pos g (Point (r - 1) c)
  | dir == E = Pos g (Point r (c + 1))
  | dir == S = Pos g (Point (r + 1) c)
  | dir == W = Pos g (Point r (c - 1))

next :: Position -> Direction -> (Position, Direction)
next curr fromDirection = case (fromDirection, currTile curr) of
  (N, NS) -> (move curr S, N)
  (S, NS) -> (move curr N, S)
  (E, EW) -> (move curr W, E)
  (W, EW) -> (move curr E, W)
  (N, NE) -> (move curr E, W)
  (E, NE) -> (move curr N, S)
  (N, NW) -> (move curr W, E)
  (W, NW) -> (move curr N, S)
  (S, SW) -> (move curr W, E)
  (W, SW) -> (move curr S, N)
  (S, SE) -> (move curr E, W)
  (E, SE) -> (move curr S, N)
  _       -> error $ "This shouldn't happen :( with " ++ show curr ++ " " ++ show fromDirection

countLoopLength :: Position -> Direction -> Point -> Int -> Int
countLoopLength pos fromDirection start count
  | count /= 0 && point pos == start = count
  -- Choice of direction in start is arbitrary
  | otherwise                        = countLoopLength nextPos fromDir start (count + 1)
  where
    (nextPos, fromDir) = next pos fromDirection

wallUp :: Position -> Point -> Position
wallUp (Pos g pt) (Point r c) = Pos (g // [(r, newRow)]) pt
  where
    newRow = (g ! r) // [(c, Wall)]

inBounds :: Grid -> Point -> Bool
inBounds g (Point rr cc) = rr >= 0 && rr < V.length g && cc >= 0 && cc < V.length (g ! 0)

-- This actually either gets insiders or outsiders depending on the direction of travel but w/e
getInsiders :: Position -> Direction -> [Point]
getInsiders pos@(Pos g (Point r c)) fromDirection = case (currTile pos, fromDirection) of
  (NS, N) -> [Point r (c - 1)]
  (NS, S) -> [Point r (c + 1)]
  (EW, E) -> [Point (r - 1) c]
  (EW, W) -> [Point (r + 1) c]
  (NE, N) -> [Point rr cc | (rr, cc) <- [(r + 1, c), (r, c - 1)], inBounds g (Point rr cc)]
  (NE, E) -> []
  (NW, N) -> []
  (NW, W) -> [Point rr cc | (rr, cc) <- [(r + 1, c), (r, c + 1)], inBounds g (Point rr cc)]
  (SW, S) -> [Point rr cc | (rr, cc) <- [(r - 1, c), (r, c + 1)], inBounds g (Point rr cc)]
  (SW, W) -> []
  (SE, S) -> []
  (SE, E) -> [Point rr cc | (rr, cc) <- [(r - 1, c), (r, c - 1)], inBounds g (Point rr cc)]
  _       -> error $ "This shouldn't happen (getInsiders) " ++ show (Point r c) ++ " " ++ show fromDirection

-- Return updated grid, loop and insiders
loopAround :: Position -> [Point] -> [Point] -> Direction -> Point -> Bool -> (Position, Set Point, Set Point)
loopAround pos path insiders fromDirection start firstMove
  | not firstMove && point pos == start = (wallUp pos start, S.fromList updatedPath, S.fromList insiders)
  | otherwise                           = loopAround (wallUp nextPos curr) updatedPath updatedInsiders nextFromDir start False
  where
    updatedPath = point pos:path
    updatedInsiders = insiders ++ getInsiders pos fromDirection
    curr = point pos
    (nextPos, nextFromDir) = next pos fromDirection

getStart :: [String] -> Int -> Point
getStart [] _ = error "Start not found in the board"
getStart (l:ls) r
  | 'S' `elem` l = Point r (fromJust ('S' `elemIndex` l))
  | otherwise     = getStart ls (r + 1)

-- This is the same as wall up essentially
mark :: Tile -> Grid -> Point -> Grid
mark t g (Point r c) = g // [(r, newRow)]
  where
    newRow = (g ! r) // [(c, t)]

updateWithInsiders :: Grid -> Set Point -> Grid
updateWithInsiders = foldl (mark Insider)

getNeighbours :: Grid -> Point -> [Point]
getNeighbours g (Point r c) = [np | np <- [Point (r + 1) c, Point r (c + 1), Point (r - 1) c, Point r (c - 1)], inBounds g np, available g np]
  where
    available gr p = (gr `at` p) `notElem` [Wall, Visited]

-- We can just do a search for insiders this way guaranteed
getAllInsiders :: Grid -> Point -> Grid
getAllInsiders g p
  -- Already visited? Done
  | g `at` p == Visited           = g
  -- No neighbours? Mark as visited and done
  | null neighbours               = mark Visited g p
  -- Otherwise for neighbours do magic
  | otherwise                     = foldl getAllInsiders (mark Visited g p) neighbours
  where
    neighbours = getNeighbours g p

getAllInsidersForIslands :: Grid -> [Point] -> Grid
getAllInsidersForIslands g ins = updatedGrid
  where
    updatedGrid = foldl getAllInsiders g ins

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then Prelude.head args else "inputs/day10/input.txt"
    let start = getStart (lines contents) 0
    let board = V.fromList $ map (V.fromList . map toTile) $ lines contents
    -- print $ countLoopLength (Pos board start) N start 0 `div` 2
    let (Pos g p, pth, ins) = loopAround (Pos board start) [] [] N start True
    printGrid g
    print ins
    print pth
    print $ ins `difference` pth
    let initialInsiders = ins `difference` pth
    let initialInsiderGrid = updateWithInsiders g initialInsiders
    printGrid initialInsiderGrid
    print $ getNeighbours initialInsiderGrid (Point 1 0)
    print $ getNeighbours initialInsiderGrid (Point 0 0)
    printGrid $ getAllInsiders initialInsiderGrid (Point 1 0)
    printGrid $ getAllInsidersForIslands initialInsiderGrid (S.toList initialInsiders)
    -- let walledUpGrid = grid $ makeWall (Pos board start) W start True
    -- print walledUpGrid
    -- print $ getAllNonEnclosed walledUpGrid (Point 0 0)