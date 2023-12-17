import System.Environment (getArgs)
import Data.PQueue.Min as PQ (MinQueue, empty, findMin, insert, deleteMin)
import Data.Map as M (Map, member, empty, insert, findWithDefault)
import Data.Char (digitToInt)

type Board = [[Int]]
type Point = (Int, Int)
data Direction = N | E | S | W deriving (Eq, Ord, Show)
type Moves = (Int, Direction)
type State = (Point, Moves)
type PQueue = MinQueue (Int, State, State)
-- Sample, part 1: 102

parseBoard :: [String] -> [[Int]]
parseBoard = map (map digitToInt)

moveDirection :: Point -> Point -> Direction
moveDirection (r1, c1) (r2, c2)
  | r1 < r2   = N
  | r1 > r2   = S
  | c1 < c2   = E
  | otherwise = W

at :: Board -> Point -> Int
at b (r, c) = (b !! r) !! c

potentialNeighbours :: Point -> Board -> [(Int, Point, Direction)]
potentialNeighbours (r, c) b = [(at b p, p, moveDirection (r, c) p) | p <- [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]]

inBounds :: Point -> Board -> Bool
inBounds (r, c) b = r >= 0 && r < length b && c >= 0 && c < length (head b)

isOppositeDir :: Direction -> Direction -> Bool
isOppositeDir N S = True
isOppositeDir S N = True
isOppositeDir E W = True
isOppositeDir W E = True
isOppositeDir _ _ = False

canMove :: Direction -> Moves -> Bool
canMove d (mc, md) = not (isOppositeDir d md) && not (md == d && mc >= 3)

updateMoveCount :: Moves -> Direction -> Moves
updateMoveCount (mc, md) d
  | md == d   = (mc + 1, d)
  | otherwise = (1, d)

-- Helper for debugging
retracePath :: Map State State -> State -> Point -> [Point]
retracePath m (cp, cm) t
  | cp == t   = [t]
  | otherwise = cp : retracePath m (M.findWithDefault ((0, 0), (0, N)) (cp, cm) m) t

-- Dijkstra with minimising loss, storing also current and last state in the queue
dijkstraRec :: Board -> Point -> Map State State -> PQueue -> Int
dijkstraRec b t seen q
  | (currPoint, currMoves) `M.member` seen = dijkstraRec b t seen (deleteMin q)
  | currPoint == t                         = currLoss
  | otherwise                              = dijkstraRec b t (M.insert (currPoint, currMoves) (lastPoint, lastMoves) seen) newQ
  where
    (currLoss, (currPoint, currMoves), (lastPoint, lastMoves)) = findMin q
    neighbours = [(currLoss + l, (p, updateMoveCount currMoves d), (currPoint, currMoves)) | (l, p, d) <- potentialNeighbours currPoint b, inBounds p b, canMove d currMoves, not (M.member (p, updateMoveCount currMoves d) seen)]
    newQ = foldl (flip PQ.insert) (deleteMin q) neighbours

dijkstra :: Board -> Point -> Point -> Int
dijkstra b s t = dijkstraRec b t M.empty (PQ.insert (0, (s, (0, N)), ((0, 0), (0, N))) PQ.empty)

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day17/input.txt"
    let board = parseBoard $ lines contents
    let start = (0, 0)
    let end = (length board - 1, length (head board) - 1)
    print $ dijkstra board start end