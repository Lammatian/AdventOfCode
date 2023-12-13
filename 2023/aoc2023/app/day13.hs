import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, isJust, fromJust)
import Lib (printGrid)
-- TODO: find[Vertical|Horizontal]Reflection functions are not needed, use only plural
-- TODO: Use Reflection type everywhere
-- TODO: can write findReflections
-- TODO: take care of all the yellow squiggles

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

findVerticalReflection :: Board -> Int -> Maybe Int
findVerticalReflection b c
  | c == length (head b) - 1 = Nothing
  | isVerticalReflection b c = Just (c + 1)
  | otherwise                = findVerticalReflection b (c + 1)

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

findHorizontalReflection :: Board -> Int -> Maybe Int
findHorizontalReflection b r
  | r == length b - 1                  = Nothing
  | isHorizontalReflection b r (r + 1) = Just (r + 1)
  | otherwise                          = findHorizontalReflection b (r + 1)

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
    newRow = take c row ++ [swap (row !! c)] ++ drop (c + 1) row

findNewReflection :: Board -> Reflection -> Reflection
findNewReflection b rf@(R rt i) = go b 0 0 rf
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

reflValue :: Reflection -> Int
reflValue (R Vert x) = x
reflValue (R Hor x) = 100 * x

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day13/input.txt"
    let boards = map lines $ splitOn "\n\n" contents
    let cols = mapMaybe (`findVerticalReflection` 0) boards
    let rows = mapMaybe (`findHorizontalReflection` 0) boards
    print $ 100 * sum rows + sum cols
    let boardVertRefs = [(b, R Vert (fromJust refl)) | b <- boards, let refl = findVerticalReflection b 0, isJust refl]
    let boardHorRefs = [(b, R Hor (fromJust refl)) | b <- boards, let refl = findHorizontalReflection b 0, isJust refl]
    let boardRefs = boardHorRefs ++ boardVertRefs
    print $ sum $ map (reflValue . uncurry findNewReflection) boardRefs
    printGrid (head boards)
