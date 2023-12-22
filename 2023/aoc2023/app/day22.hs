import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sortOn)
import Data.Map (Map, member, insert, empty, assocs, insertWith, (!))
import qualified Data.Map as M (map, filter)
import Data.Set (Set, fromList, toList, union, singleton, findMin, size, delete)

type Point = (Int, Int, Int)
type Brick = (Point, Point)
-- Map from point to brick 'ID'
type Stack = Map Point Int
type Support = Map Int (Set Int)

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

lowerBrick :: Brick -> Brick
lowerBrick (p1, p2) = (lowerPoint p1, lowerPoint p2)

supports :: Stack -> Brick -> Bool
supports s b = any (`member` s) oneLower || brickZ b == 1
  where
    oneLower = cubes $ lowerBrick b

addBrickToStack :: Brick -> Int -> Stack -> Stack
addBrickToStack b i s = foldl (\acc x -> insert x i acc) s $ cubes b

fall :: Stack -> Brick -> Int -> Stack
fall s b i
  | s `supports` b = addBrickToStack b i s
  | otherwise      = fall s (lowerBrick b) i

fallAllRec :: Stack -> [(Brick, Int)] -> Stack
fallAllRec = foldl (\s (b, i) -> fall s b i)

fallAll :: Stack -> [Brick] -> Stack
fallAll s bs = fallAllRec s $ zip bs [1..]

getSupportsRec :: [(Point, Int)] -> Stack -> Support
getSupportsRec [] _ = empty
getSupportsRec ((p, i):ps) m
  | lp `member` m && supporterIdx /= i = insertWith union i (singleton supporterIdx) (getSupportsRec ps m)
  | otherwise                          = getSupportsRec ps m
  where
    lp = lowerPoint p
    supporterIdx = m!lp

-- Returns the mapping from brick ID to IDs of bricks that support it
getSupports :: Stack -> Support
getSupports s = getSupportsRec (assocs s) s

nonRemovable :: Support -> [Int]
nonRemovable m = toList $ fromList $ [findMin v | (_, v) <- assocs m, size v == 1]

-- This is still disgustingly slow
removeSupport :: Support -> Int -> (Support, [Int])
removeSupport s i = (newM, falling)
  where
    newM = M.map (delete i) s
    falling = [k | (k, v) <- assocs s, size v == 1, findMin v == i]

chainReactionRec :: Support -> [Int] -> Int
chainReactionRec m [] = length $ M.filter null m
chainReactionRec m (i:is) = chainReactionRec newM (is ++ newFalling)
  where
    (newM, newFalling) = removeSupport m i

chainReaction :: Support -> Int -> Int
chainReaction s i = r
  where
    r = chainReactionRec s [i]

chainReactions :: Support -> [Int] -> Int
chainReactions s is = sum $ map (chainReaction s) is

-- IDEA: Traverse the Support graph such that for each element the 'base' supports are stored
--       then it's easy to determine which ones fall when a given element is removed
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
    let nRemovable = nonRemovable supps
    print $ length zSortedBricks - length nRemovable
    print $ chainReactions supps nRemovable
