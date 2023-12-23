import System.Environment (getArgs)
import Data.Set as St (Set, notMember, insert, empty)
import Data.Map as M (Map, empty, fromList, (!))
import Data.Maybe (catMaybes)

type Board = [String]
type Point = (Int, Int)
-- Edges of a graph
type Node = Point
type Graph = Map Node [(Node, Int)]
type Nodes = [Node]

at :: Board -> Point -> Char
at b (r, c) = (b!!r)!!c

forcedNext :: Char -> Point -> Point
forcedNext '>' (r, c) = (r, c + 1)
forcedNext 'v' (r, c) = (r + 1, c)
forcedNext '<' (r, c) = (r, c - 1)
forcedNext '^' (r, c) = (r - 1, c)
forcedNext _ _ = error "Do not call me that"

getNodes :: Board -> Point -> Point -> [Point]
getNodes b s e = [(r, c) | r <- [0..rr - 1], c <- [0..cc - 1], notWall (r, c), isNode (r, c)] ++ [s, e]
  where
    rr = length b
    cc = length (head b)
    inBounds (r, c) = r >= 0 && r < rr && c >= 0 && c < cc
    notWall p = b `at` p /= '#'
    ns (r, c) = filter (\p -> inBounds p && notWall p) [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
    isNode p = length (ns p) > 2

neighbours :: Board -> Bool -> Point -> [Point]
neighbours b slippery (r, c) = [n | n <- allDirs, inBounds n, not (isWall n), passable n]
  where
    allDirs = [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
    inBounds (r', c') = r' >= 0 && r' < length b && c' >= 0 && c' < length (head b)
    isWall n = b `at` n == '#'
    passable n = not slippery || b `at` n `notElem` "<>v^" || forcedNext (b `at` n) n /= (r, c)

findNode :: Board -> Bool -> Point -> Point -> Int -> Nodes -> Maybe (Point, Int)
findNode b slippery f p d ns
  | p `elem` ns       = Just (p, d)
  | not (null nextPs) = findNode b slippery p (head nextPs) (d + 1) ns
  | otherwise         = Nothing
  where
    -- Assume there's always just one, otherwise would be in node
    nextPs = [n | n <- neighbours b slippery p, n /= f]

findNeighbourNodes :: Board -> Bool -> Node -> Nodes -> [(Node, Int)]
findNeighbourNodes b slippery s ns = catMaybes [findNode b slippery s n 1 ns | n <- neighbours b slippery s]

buildGraph :: Board -> Nodes -> Bool -> Graph
buildGraph _ [] _ = M.empty
buildGraph b ns slippery = M.fromList [(n, findNeighbourNodes b slippery n ns) | n <- ns]

-- This is surprisingly slow given how small the graph is lol
longestPath :: Graph -> Node -> Int -> Node -> Set Node -> Int
longestPath g p d t seen
  | p == t    = d
  | otherwise = maximum $ 0:[longestPath g n (d + d') t (insert n seen) | (n, d') <- g!p, n `notMember` seen]

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day23/input.txt"
    let grid = lines contents
    let start = (0, 1)
    let end = (length grid - 1, length (head grid) - 2)
    let nodes = getNodes grid start end
    let graph1 = buildGraph grid nodes True
    print $ longestPath graph1 start 0 end St.empty
    let graph2 = buildGraph grid nodes False
    print $ longestPath graph2 start 0 end St.empty