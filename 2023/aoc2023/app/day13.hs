import System.Environment (getArgs)
import Data.List.Split (splitOn)

type Board = [String]
data ReflType = Vert | Hor deriving (Eq, Show)
data Reflection = R {
  reflType :: ReflType,
  idx :: Int
} deriving (Eq, Show)

isVerticalReflection :: Board -> Int -> Bool
isVerticalReflection b c = and compCols
  where
    refl = min (c + 1) (length (head b) - (c + 1))
    left = map (take refl . reverse . take (c + 1)) b
    right = map (take refl . drop (c + 1)) b
    compCols = zipWith (==) left right

findVerticalReflections :: Board -> Int -> [Reflection]
findVerticalReflections b c
  | c == length (head b) - 1 = []
  | isVerticalReflection b c = R Vert (c + 1) : findVerticalReflections b (c + 1)
  | otherwise                = findVerticalReflections b (c + 1)

isHorizontalReflection :: Board -> Int -> Int -> Bool
isHorizontalReflection b u d
  | u == 0 || d == length b - 1 = areMatchingRows
  | otherwise                   = areMatchingRows && isHorizontalReflection b (u - 1) (d + 1)
  where
    areMatchingRows = (b !! u) == (b !! d)

findHorizontalReflections :: Board -> Int -> [Reflection]
findHorizontalReflections b r
  | r == length b - 1                  = []
  | isHorizontalReflection b r (r + 1) = R Hor (r + 1) : findHorizontalReflections b (r + 1)
  | otherwise                          = findHorizontalReflections b (r + 1)

replace :: Board -> Int -> Int -> Board
replace b r c = take r b ++ [newRow] ++ drop (r + 1) b
  where
    row = b !! r
    swap '#' = '.'
    swap '.' = '#'
    swap chr = error $ "Unexpected character in board: " ++ [chr]
    newRow = take c row ++ [swap (row !! c)] ++ drop (c + 1) row

-- Brute force: try to replace each spot in the board and see if that produces a new reflection
-- Another approach would be to count the number of differences in a reflection, but cba lol
findNewReflection :: Board -> Reflection -> Reflection
findNewReflection board = go board 0 0
  where
    go b r c rf
      | c >= length (head b) || r >= length b = error $ "Why did this happen " ++ show r ++ " " ++ show c
      | not (null horizontal) = head horizontal
      | not (null vertical)   = head vertical
      | otherwise             = go b newR newC rf
      where
        newBoard = replace b r c
        horizontal = filter (/= rf) $ findHorizontalReflections newBoard 0
        vertical = filter (/= rf) $ findVerticalReflections newBoard 0
        (newR, newC) = if c == length (head b) - 1 then (r + 1, 0) else (r, c + 1)

findOriginalReflection :: Board -> Reflection
findOriginalReflection b
  | not (null horizontal) = head horizontal
  | not (null vertical)   = head vertical
  | otherwise             = error "This shouldn't happen"
  where
    horizontal = findHorizontalReflections b 0
    vertical = findVerticalReflections b 0

reflValue :: Reflection -> Int
reflValue (R Vert x) = x
reflValue (R Hor x) = 100 * x

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day13/input.txt"
    let boards = map lines $ splitOn "\n\n" contents
    let refls = [(b, findOriginalReflection b) | b <- boards]
    print $ sum $ map (reflValue . snd) refls
    print $ sum $ map (reflValue . uncurry findNewReflection) refls