import System.Environment (getArgs)
import Data.Sequence (Seq((:|>), Empty, (:<|)), (><), mapWithIndex, fromList)
import Debug.Trace (trace)

type Board = [String]
type Point = (Int, Int)

at :: Board -> Point -> Char
at b (r, c) = (b!!r)!!c

forcedNext :: Char -> Point -> Point
forcedNext '>' (r, c) = (r, c + 1)
forcedNext 'v' (r, c) = (r + 1, c)
forcedNext '<' (r, c) = (r, c - 1)
forcedNext '^' (r, c) = (r - 1, c)
forcedNext _ _ = error "Do not call me that"

neighbours :: Board -> Point -> Point -> Seq Point
neighbours b f p@(r, c) = fromList $ [n | n <- allDirs, inBounds n, n /= f, not (useless n), not (isWall n)]
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

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day23/input.txt"
    let grid = lines contents
    let start = (0, 1)
    let end = (length grid - 1, length (head grid) - 2)
    print $ longest grid end (Empty:|>((-1, 0), start, 0))