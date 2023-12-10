import System.Environment (getArgs)
import Data.Vector as V (Vector, fromList, toList, (!), (//), length, head, tail, map, imap, imapMaybe)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set as S (Set, fromList, difference, toList, empty, union, member, size)

type Grid = Vector (Vector Tile)
data Direction = N | E | S | W deriving (Show, Eq)
data Tile = NS | EW | NE | NW | SW | SE | Invalid | Wall | InitialInsider | VisitedInsider deriving (Eq)
data Point = Point {
  row :: Int,
  col :: Int
} deriving (Show, Eq, Ord)
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
  show InitialInsider = "I"
  show VisitedInsider = "V"

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

getStart :: [String] -> Int -> Point
getStart [] _ = error "Start not found in the board"
getStart (l:ls) r
  | 'S' `elem` l = Point r (fromJust ('S' `elemIndex` l))
  | otherwise     = getStart ls (r + 1)

dirs :: Tile -> [Direction]
dirs NS = [N, S]
dirs EW = [E, W]
dirs NE = [N, E]
dirs NW = [N, W]
dirs SW = [S, W]
dirs SE = [S, E]
dirs _  = []

points :: Tile -> Direction -> Bool
points t d = d `elem` dirs t

isConnected :: Grid -> Point -> Direction -> Bool
isConnected g (Point r c) N = points (g `at` Point (r - 1) c) S
isConnected g (Point r c) E = points (g `at` Point r (c + 1)) W
isConnected g (Point r c) S = points (g `at` Point (r + 1) c) N
isConnected g (Point r c) W = points (g `at` Point r (c - 1)) E

-- TODO: Assumes start is not on the edge
detectStartTile :: Grid -> Point -> Tile
detectStartTile g p = case neswConnections of
  [True, True, _, _] -> NE
  [True, _, True, _] -> NS
  [True, _, _, True] -> NW
  [_, True, True, _] -> SE
  [_, True, _, True] -> EW
  [_, _, True, True] -> SW
  _                  -> error $ "Impossible bruh " ++ show neswConnections
  where
    neswConnections = [isConnected g p d | d <- [N, E, S, W]]

at :: Grid -> Point -> Tile
g `at` Point r c = (g ! r) ! c

currTile :: Position -> Tile
currTile (Pos g p) = at g p

move :: Position -> Direction -> Position
move (Pos g (Point r c)) N = Pos g (Point (r - 1) c)
move (Pos g (Point r c)) E = Pos g (Point r (c + 1))
move (Pos g (Point r c)) S = Pos g (Point (r + 1) c)
move (Pos g (Point r c)) W = Pos g (Point r (c - 1))

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

wallUp :: Position -> Point -> Position
wallUp (Pos g pt) (Point r c) = Pos (g // [(r, newRow)]) pt
  where
    newRow = (g ! r) // [(c, Wall)]

inBounds :: Grid -> Point -> Bool
inBounds g (Point rr cc) = rr >= 0 && rr < V.length g && cc >= 0 && cc < V.length (g ! 0)

-- This actually either gets insiders or outsiders depending on the direction of travel but w/e
getPotentialInsiders :: Position -> Direction -> [Point]
getPotentialInsiders pos@(Pos g (Point r c)) fromDirection = case (currTile pos, fromDirection) of
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
  _       -> error $ "This shouldn't happen (getPotentialInsiders) " ++ show (Point r c) ++ " " ++ show fromDirection

-- Return updated grid, loop and insiders
loopAround :: Position -> [Point] -> [Point] -> Direction -> Point -> Bool -> (Position, Set Point, Set Point)
loopAround pos path insiders fromDirection start firstMove
  | not firstMove && point pos == start = (wallUp pos start, S.fromList updatedPath, S.fromList insiders)
  | otherwise                           = loopAround (wallUp nextPos curr) updatedPath updatedInsiders nextFromDir start False
  where
    updatedPath = point pos:path
    updatedInsiders = insiders ++ getPotentialInsiders pos fromDirection
    curr = point pos
    (nextPos, nextFromDir) = next pos fromDirection

-- This is the same as wall up essentially
mark :: Tile -> Grid -> Point -> Grid
mark t g (Point r c) = g // [(r, newRow)]
  where
    newRow = (g ! r) // [(c, t)]

updateWithInsiders :: Grid -> Set Point -> Grid
updateWithInsiders = foldl (mark InitialInsider)

getNeighbours :: Grid -> Point -> [Point]
getNeighbours g (Point r c) = [np | np <- [Point (r + 1) c, Point r (c + 1), Point (r - 1) c, Point r (c - 1)], inBounds g np, available g np]
  where
    available gr p = (gr `at` p) `notElem` [Wall, VisitedInsider]

-- We can just do a search for insiders this way guaranteed
markInsidersFromPoint :: Grid -> Point -> Grid
markInsidersFromPoint g p
  -- Already visited? Done
  | g `at` p == VisitedInsider    = g
  -- No neighbours? Mark as visited and done
  | null neighbours               = mark VisitedInsider g p
  -- Otherwise for neighbours do magic
  | otherwise                     = foldl markInsidersFromPoint (mark VisitedInsider g p) neighbours
  where
    neighbours = getNeighbours g p

markAllInsiders :: Grid -> [Point] -> Grid
markAllInsiders g ins = updatedGrid
  where
    updatedGrid = foldl markInsidersFromPoint g ins

getInsidersFromGrid :: Grid -> Set Point
getInsidersFromGrid g = foldl S.union S.empty pointSets
  where
    insiderIndices = V.map (V.imapMaybe (\i t -> if t == VisitedInsider then Just i else Nothing)) g
    toPointSet rowIdx colIdxs = S.fromList $ V.toList $ V.map (Point rowIdx) colIdxs
    pointSets = V.toList $ V.imap toPointSet insiderIndices

getInsiderCount :: Grid -> Set Point -> Int
getInsiderCount g path = if Point 0 0 `member` insiders
  then gsize - S.size insiders - S.size path
  else S.size insiders
  where
    gsize = V.length g * V.length (g ! 0)
    insiders = getInsidersFromGrid g

-- TODO: Is Position type necessary?
-- TODO: wallUp is unnecessary (mark can replace it probably)
-- TODO: This solution assumes (0, 0) is not part of the pipe
main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then Prelude.head args else "inputs/day10/input.txt"
    let startPos = getStart (lines contents) 0
    let board = V.fromList $ Prelude.map (V.fromList . Prelude.map toTile) $ lines contents
    let startTile = detectStartTile board startPos
    let boardNoS = mark startTile board startPos
    let (Pos g _, pth, ins) = loopAround (Pos boardNoS startPos) [] [] (Prelude.head (dirs startTile)) startPos True
    print $ Prelude.length (S.toList pth) `div` 2
    let initialInsiders = ins `difference` pth
    let initialInsiderGrid = updateWithInsiders g initialInsiders
    let fullGrid = markAllInsiders initialInsiderGrid (S.toList initialInsiders)
    print $ getInsiderCount fullGrid pth