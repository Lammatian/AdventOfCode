import System.Environment (getArgs)
import Data.Sequence as Sq (Seq((:|>), Empty, (:<|)), (><), mapWithIndex, fromList)
import Data.Set as St (Set, notMember, insert, empty)
import Data.Map as M (Map, empty, fromList, (!))
import Debug.Trace (trace)

type Board = [String]
type Point = (Int, Int)
-- Edges of a graph
type Graph = Map Point [(Point, Int)]
type Nodes = [Point]

at :: Board -> Point -> Char
at b (r, c) = (b!!r)!!c

forcedNext :: Char -> Point -> Point
forcedNext '>' (r, c) = (r, c + 1)
forcedNext 'v' (r, c) = (r + 1, c)
forcedNext '<' (r, c) = (r, c - 1)
forcedNext '^' (r, c) = (r - 1, c)
forcedNext _ _ = error "Do not call me that"

neighbours :: Board -> Point -> Point -> Seq Point
neighbours b f p@(r, c) = Sq.fromList $ [n | n <- allDirs, inBounds n, n /= f, not (useless n), not (isWall n)]
  where
    allDirs = [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
    isWall n = b `at` n == '#'
    inBounds (nr, nc) = nr >= 0 && nr < length b && nc >= 0 && nc < length (head b)
    useless n = b `at` n `elem` "<>^v" && forcedNext (b `at` n) n == p

-- BFS
longest :: Board -> Point -> Seq (Point, Point, Int) -> Int
longest _ _ Empty = 0
longest b t ((f, p, d):<|ss)
  | p == t          = max d $ longest b t ss
  | c `elem` "<>^v" = longest b t (ss:|>(p, forcedNext c p, d + 1))
  | otherwise       = longest b t (ss><ns)
  where
    c = b `at` p
    ns = mapWithIndex (\_ a -> (p, a, d + 1)) $ neighbours b f p

neighbours2 :: Board -> Set Point -> Point -> Seq Point
neighbours2 b seen (r, c) = Sq.fromList $ [n | n <- allDirs, inBounds n, n `notMember` seen, not (isWall n)]
  where
    allDirs = [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
    isWall n = b `at` n == '#'
    inBounds (nr, nc) = nr >= 0 && nr < length b && nc >= 0 && nc < length (head b)

-- DFS
longest2 :: Board -> Point -> Point -> Set Point -> Int
longest2 b t p seen
  | p == t = length seen
  | otherwise = case ns of
      Empty -> 0
      (n:<|Empty) -> longest2 b t n (insert p seen)
      (n1:<|n2:<|Empty) -> maximum [longest2 b t n (insert p seen) | n <- [n1, n2]]
      (n1:<|n2:<|n3:<|Empty) -> maximum [longest2 b t n (insert p seen) | n <- [n1, n2, n3]]
      _ -> error "Too many neighbours lol"
  where
    ns = neighbours2 b seen p

getNodes :: Board -> Point -> Point -> [Point]
getNodes b s e = [(r, c) | r <- [0..rr - 1], c <- [0..cc - 1], notWall (r, c), isNode (r, c)] ++ [s, e]
  where
    rr = length b
    cc = length (head b)
    inBounds (r, c) = r >= 0 && r < rr && c >= 0 && c < cc
    notWall p = b `at` p /= '#'
    ns (r, c) = filter (\p -> inBounds p && notWall p) [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
    isNode p = length (ns p) > 2

simpleNeighbours :: Board -> Point -> [Point]
simpleNeighbours b (r, c) = [n | n <- allDirs, inBounds n, not (isWall n)]
  where
    allDirs = [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
    inBounds (r', c') = r' >= 0 && r' < length b && c' >= 0 && c' < length (head b)
    isWall n = b `at` n == '#'

findNode :: Board -> Point -> Point -> Int -> Nodes -> (Point, Int)
findNode b f p d ns
  | p `elem` ns = (p, d)
  | otherwise   = findNode b p nextP (d + 1) ns
  where
    -- Assume there's always just one, otherwise would be in node
    nextP = head $ [n | n <- simpleNeighbours b p, n /= f]

findNeighbours :: Board -> Point -> Nodes -> [(Point, Int)]
findNeighbours b s ns = [findNode b s n 1 ns | n <- simpleNeighbours b s]

buildGraph :: Board -> Nodes -> Graph
buildGraph _ [] = M.empty
buildGraph b ns = M.fromList [(n, findNeighbours b n ns) | n <- ns]

longestPath :: Graph -> Point -> Point -> Set Point -> Int
longestPath g s e visited
  | s == e    = 0
  | otherwise = maximum $ 0:[d + longestPath g s n (insert n visited) | (n, d) <- g!e, n `notMember` visited]

-- Part 2: Either DFS or construct a graph
main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day23/input.txt"
    let grid = lines contents
    let start = (0, 1)
    let end = (length grid - 1, length (head grid) - 2)
    print $ longest grid end (Empty:|>((-1, 0), start, 0))
    -- print $ longest2 grid end start empty
    let nodes = getNodes grid start end
    print nodes
    let graph = buildGraph grid nodes
    print graph
    print $ longestPath graph start end St.empty
    -- print $ findNeighbours grid start nodes