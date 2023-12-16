import System.Environment (getArgs)
import Data.Set (Set, insert, empty, member, map)
import Control.Monad.State.Lazy (State, get, put, evalState)

data Direction = N | E | S | W deriving (Eq, Ord, Show)
type Point = (Int, Int)
type Board = [String]

at :: Board -> Point -> Char
at b (r, c) = (b !! r) !! c

move :: Point -> Direction -> Point
move (r, c) N = (r - 1, c)
move (r, c) E = (r, c + 1)
move (r, c) S = (r + 1, c)
move (r, c) W = (r, c - 1)

inBounds :: Point -> Board -> Bool
inBounds (r, c) b = r >= 0 && r < rr && c >= 0 && c < cc
  where
    rr = length b
    cc = length (head b)

newTrace :: Board -> Point -> Direction -> State (Set (Point, Direction)) ()
newTrace b p d = do
  s <- get
  let isMember = (p, d) `member` s
  let inBds = p `inBounds` b
  case not inBds || isMember of
    True -> do return ()
    False -> do
      -- :( how can I nicely call as many as needed?
      put $ (p, d) `insert` s
      newTrace b (move p (head newDs)) (head newDs)
      newTrace b (move p (last newDs)) (last newDs)
      return ()
      where
        newDs = case (b `at` p, d) of
          ('\\', N) -> [W]
          ('\\', E) -> [S]
          ('\\', S) -> [E]
          ('\\', W) -> [N]
          ('/', N)  -> [E]
          ('/', E)  -> [N]
          ('/', S)  -> [W]
          ('/', W)  -> [S]
          ('|', E)  -> [N, S]
          ('|', W)  -> [N, S]
          ('-', N)  -> [E, W]
          ('-', S)  -> [E, W]
          _         -> [d]

-- Set Point, Djokovic XD
getResult :: Board -> Point -> Direction -> State (Set (Point, Direction)) (Set Point)
getResult b p d = do
  newTrace b p d
  Data.Set.map fst <$> get

possibleStarts :: Int -> Int -> [(Point, Direction)]
possibleStarts r c = top ++ right ++ bottom ++ left ++ corners
  where
    top     = [((0, c'), S) | c' <- [1..c-1]]
    right   = [((r', c - 1), W) | r' <- [1..r-1]]
    bottom  = [((r - 1, c'), N) | c' <- [1..c-1]]
    left    = [((r', 0), E) | r' <- [1..r-1]]
    corners = [((0, 0), E), ((0, 0), S), ((0, c - 1), W), ((0, c - 1), S), ((r - 1, 0), E), ((r - 1, 0), N), ((r - 1, c - 1), W), ((r - 1, c - 1), N)]

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day16/input.txt"
    let board = lines contents
    let test = evalState (getResult board (0, 0) E) empty
    print $ length test
    let starts = possibleStarts (length board) (length (head board))
    let tests = Prelude.map (\(p, d) -> length $ evalState (getResult board p d) empty) starts
    print $ maximum tests