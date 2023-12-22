import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sortOn, delete)
import Data.Map (Map, member, insert, empty, assocs, insertWith, (!), keys, elems)
import qualified Data.Map as M (map, filter)
import Data.Set (fromList, toList)
import Debug.Trace (trace)

type Point = (Int, Int, Int)
type Brick = (Point, Point)
-- Map from point to brick 'ID'
type Stack = Map Point Int

parsePoint :: String -> Point
parsePoint s = (x, y, z)
  where
    [x, y, z] = map read $ splitOn "," s

parseBrick :: String -> Brick
parseBrick s = (parsePoint s1, parsePoint s2)
  where
    [s1, s2] = splitOn "~" s

cubes :: Brick -> [Point]
cubes (b1@(x1, y1, z1), b2@(x2, y2, z2))
  | x1 < x2 = [(x, y1, z1) | x <- [x1..x2]]
  | y1 < y2 = [(x1, y, z1) | y <- [y1..y2]]
  | z1 < z2 = [(x1, y1, z) | z <- [z1..z2]]
  | x1 == x2 && y1 == y2 && z1 == z2 = [(x1, y1, z1)]
  | otherwise = error $ "Weird brick " ++ show b1 ++ show b2

brickZ :: Brick -> Int
brickZ ((_, _, z), _) = z

lowerPoint :: Point -> Point
lowerPoint (x, y, z) = (x, y, z - 1)

lower :: Brick -> Brick
lower (p1, p2) = (lowerPoint p1, lowerPoint p2)

supports :: Stack -> Brick -> Bool
-- supports s b
--   | brickZ b == 1 = Just [0]
--   | otherwise     = if null sups then Nothing else Just sups
--   where
--     oneLower = cubes $ lower b
--     sups = [v | (k, v) <- assocs s, k `elem` oneLower]
supports s b = any (`member` s) oneLower || brickZ b == 1
  where
    oneLower = cubes $ lower b

addBrickToStack :: Brick -> Int -> Stack -> Stack
addBrickToStack b i s = foldl (\acc x -> insert x i acc) s $ cubes b

fall :: Stack -> Brick -> Int -> Stack
fall s b i
  | supports s b = addBrickToStack b i s
  | otherwise    = fall s (lower b) i

fallAllRec :: Stack -> [(Brick, Int)] -> Stack
fallAllRec s [] = s
fallAllRec s ((b, i):bs) = fallAllRec (fall s b i) bs

fallAll :: Stack -> [Brick] -> Stack
fallAll s bs = fallAllRec s $ zip bs [1..]

getSupportsRec :: [(Point, Int)] -> Stack -> Map Int [Int]
getSupportsRec [] _ = empty
getSupportsRec ((p, i):ps) m
  | lp `member` m && supporterIdx /= i = insertWith (\curr new -> if head new `elem` curr then curr else new ++ curr) i [supporterIdx] (getSupportsRec ps m)
  | otherwise                          = getSupportsRec ps m
  where
    lp = lowerPoint p
    supporterIdx = m!lp

-- Returns the mapping from brick ID to IDs of bricks that support it
getSupports :: Stack -> Map Int [Int]
getSupports s = getSupportsRec (assocs s) s

nonRemovable :: Map Int [Int] -> [Int]
nonRemovable m = toList $ fromList $ [head v | (_, v) <- assocs m, length v == 1]

-- This is still disgustingly slow
removeSupport :: Map Int [Int] -> Int -> (Map Int [Int], [Int])
removeSupport m i = (newM, falling)
  where
    newM = M.map (delete i) m
    falling = [k | (k, v) <- assocs m, length v == 1, head v == i]

chainReactionRec :: Map Int [Int] -> [Int] -> Int
chainReactionRec m [] = length $ M.filter null m
chainReactionRec m (i:is) = chainReactionRec newM (is ++ newFalling)
  where
    (newM, newFalling) = removeSupport m i

chainReaction :: Map Int [Int] -> Int -> Int
chainReaction m i = r
  where
    r = chainReactionRec m [i]

chainReactions :: Map Int [Int] -> [Int] -> Int
chainReactions m is = sum $ map (chainReaction m) is

getActualSupport :: Map Int [Int] -> Int -> [Int]
getActualSupport m i

getActualSupports :: Map Int [Int] -> Map Int [Int]
getActualSupports m = getActualSupportsRec m (keys m)

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day22/input.txt"
    let bricks = map parseBrick $ lines contents
    -- Assume left point is <= right point
    let zSortedBricks = sortOn (\((_, _, z), _) -> z) bricks
    let stacked = fallAll empty zSortedBricks
    let supps = getSupports stacked
    print supps
    let nRemovable = nonRemovable supps
    print $ length zSortedBricks - length nRemovable
    print $ chainReactions supps nRemovable
