{-# LANGUAGE TupleSections #-}
import System.Environment (getArgs)
import Lib (printGrid)
import Data.Set as St (Set)
import Data.Map as M (Map, empty, keysSet, member, insert, elems, fromList, (!), assocs)
import qualified Data.Map as M (map, filter)
import Data.Sequence as Sq (empty, Seq ((:|>), (:<|), Empty), (><), fromList)
import Data.List (findIndex, elemIndex, sort)
import Data.Maybe (fromJust)
import qualified Data.Vector as V (Vector, fromList, (!), length, map, slice, filter)
import Debug.Trace (trace)

type Board = [String]
type Point = (Int, Int)
-- This is the same declaration, but this is the coordinates of the full copies of the map
type BoxPoint = (Int, Int)
type Distance = Int
data Direction = N | NE | E | SE | S | SW | W | NW deriving (Eq, Ord)

getStart :: Board -> Point
getStart b = (r, c)
  where
    r = fromJust $ findIndex (elem 'S') b
    c = fromJust $ elemIndex 'S' (b!!r)

at :: Board -> Point -> Char
at b (r, c) = (b!!mr)!!mc
  where
    mr = r `mod` length b
    mc = c `mod` length (head b)

isReachable :: Board -> Point -> Bool
isReachable b p@(r, c) = notRock && inBounds
  where
    notRock = b `at` p /= '#'
    rr = length b
    cc = length (head b)
    inBounds = r >= 0 && r < rr && c >= 0 && c < cc

neighbours :: Board -> Point -> [Point]
neighbours b (r, c) = [(r + dr, c + dc) | dr <- [-1,0,1], dc <- [-1,0,1], dr * dc == 0, isReachable b (r + dr, c + dc)]

bfsRec :: Board -> Distance -> Seq (Point, Distance) -> Map Point Distance -> Set Point
bfsRec _ maxD Sq.Empty seen = keysSet $ M.filter ((==(maxD `mod` 2)) . (`mod` 2)) seen
bfsRec b maxD ((p, d):<|ss) seen
  | p `member` seen = bfsRec b maxD ss seen
  | d == maxD       = bfsRec b maxD ss (M.insert p d seen)
  | otherwise       = bfsRec b maxD (ss><ns) (M.insert p d seen)
  where
    ns = Sq.fromList $ map (, d + 1) $ neighbours b p

bfs :: Board -> Distance -> Set Point
bfs b maxD = bfsRec b maxD (Sq.empty:|>(start, 0)) M.empty
  where
    start = getStart b

bfs2 :: Board -> Seq (Point, Distance) -> Map Point Distance -> [Distance]
bfs2 _ Sq.Empty seen = sort $ M.elems seen
bfs2 b ((p, d):<|ss) seen
  | p `member` seen = bfs2 b ss seen
  | otherwise       = bfs2 b (ss><ns) (M.insert p d seen)
  where
    ns = Sq.fromList $ map (, d + 1) $ neighbours b p

-- Assume square board
shortest :: Board -> Direction -> [Distance]
shortest b dir = case dir of
  N  -> map (\d -> d + hl + 1) $ bfs2 b (Sq.empty:|>((l - 1, hl), 0)) M.empty
  NE -> map (\d -> d + l + 2) $ bfs2 b (Sq.empty:|>((l - 1, 0), 0)) M.empty
  E  -> map (\d -> d + hl + 1) $ bfs2 b (Sq.empty:|>((hl, 0), 0)) M.empty
  SE -> map (\d -> d + l + 2) $ bfs2 b (Sq.empty:|>((0, 0), 0)) M.empty
  S  -> map (\d -> d + hl + 1) $ bfs2 b (Sq.empty:|>((0, hl), 0)) M.empty
  SW -> map (\d -> d + l + 2) $ bfs2 b (Sq.empty:|>((0, l - 1), 0)) M.empty
  W  -> map (\d -> d + hl + 1) $ bfs2 b (Sq.empty:|>((hl, l), 0)) M.empty
  NW -> map (\d -> d + l + 2) $ bfs2 b (Sq.empty:|>((l - 1, l - 1), 0)) M.empty
  where
    l = length b
    hl = l `div` 2

shortestByDir :: Board -> Map Direction [Distance]
shortestByDir b = M.fromList $ map (\dir -> (dir, shortest b dir)) [N, NE, E, SE, S, SW, W, NW]

boxDirection :: BoxPoint -> BoxPoint -> Direction
boxDirection (sourceR, sourceC) (targetR, targetC) = case (targetR `compare` sourceR, targetC `compare` sourceC) of
  (LT, EQ) -> N
  (LT, GT) -> NE
  (EQ, GT) -> E
  (GT, GT) -> SE
  (GT, EQ) -> S
  (GT, LT) -> SW
  (EQ, LT) -> W
  (LT, LT) -> NW
  (EQ, EQ) -> error "Comparing the same box points"

translatedBoxPoint :: BoxPoint -> Direction -> BoxPoint
translatedBoxPoint (br, bc) N = (br + 1, bc)
translatedBoxPoint (br, bc) NE = (br + 1, bc - 1)
translatedBoxPoint (br, bc) E = (br, bc - 1)
translatedBoxPoint (br, bc) SE = (br - 1, bc - 1)
translatedBoxPoint (br, bc) S = (br - 1, bc)
translatedBoxPoint (br, bc) SW = (br - 1, bc + 1)
translatedBoxPoint (br, bc) W = (br, bc + 1)
translatedBoxPoint (br, bc) NW = (br + 1, bc + 1)

-- Distance between the mid-points of two boxes
boxMidDistances :: Board -> BoxPoint -> BoxPoint -> Int
boxMidDistances b (br1, bc1) (br2, bc2) = length b * (abs (br1 - br2) + abs (bc1 - bc2))

isAnyReachable :: Board -> BoxPoint -> Map Direction [Distance] -> Int -> Bool
isAnyReachable b bp m limit = limit >= head (m!boxDir) + boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir)
  where
    boxDir = boxDirection (0, 0) bp

isBoxPartiallyReachable :: Board -> BoxPoint -> Map Direction [Distance] -> Int -> Bool
isBoxPartiallyReachable = isAnyReachable

areAllReachable :: Board -> BoxPoint -> Map Direction [Distance] -> Int -> Bool
areAllReachable b bp m limit = limit >= last (m!boxDir) + boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir)
  where
    boxDir = boxDirection (0, 0) bp

isBoxFullyReachable :: Board -> BoxPoint -> Map Direction [Distance] -> Int -> Bool
isBoxFullyReachable = areAllReachable

smallerEqCountRec :: V.Vector Int -> Int -> Int -> Int -> Int
smallerEqCountRec xs i a b
  | a >= b          = a
  | i >= (xs V.! m) = smallerEqCountRec xs i (m + 1) b
  | otherwise       = smallerEqCountRec xs i a m
  where
    m = a + (b - a) `div` 2

smallerEqCount :: V.Vector Int -> Int -> Int
smallerEqCount xs i = smallerEqCountRec xs i 0 (V.length xs - 1)

smallerEqRec :: V.Vector Int -> Int -> Int -> Int -> V.Vector Int
smallerEqRec xs i a b
  | a >= b          = V.slice 0 (if a > 0 then a - 1 else 0) xs
  | i >= (xs V.! m) = smallerEqRec xs i (m + 1) b
  | otherwise       = smallerEqRec xs i a m
  where
    m = a + (b - a) `div` 2

smallerEq :: V.Vector Int -> Int -> V.Vector Int
smallerEq xs i = smallerEqRec xs i 0 (V.length xs - 1)

vReachableCount :: Board -> BoxPoint -> Map (Direction, Bool) (V.Vector Distance) -> Int -> Int
vReachableCount b bp m l = smallerEqCount distances (l - boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir))
-- vReachableCount b bp m l = smallerEqCount distances (l - boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir))
  where
    boxDir = boxDirection (0, 0) bp
    -- There is NO WAY I'll understand this in like 1 hour from now XD
    oddBox = odd $ uncurry (+) bp
    oddLimit = odd l
    needOdd = if oddBox then oddLimit else not oddLimit 
    distances = m!(boxDir, needOdd)
    -- distances = V.map (+ boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir)) (m!boxDir)

-- Counts how many elements of xs are smaller than i Assumes sorted list
-- This could definitely be improved with vector lol
findIndexS :: [Int] -> Int -> Int
findIndexS [] _ = 0
findIndexS (x:xs) i
  | i >= x    = 1 + findIndexS xs i
  | otherwise = 0

trim :: [Int] -> Int -> [Int]
trim [] _ = []
trim (x:xs) i
  | i >= x    = x : trim xs i
  | otherwise = []

reachable :: Board -> BoxPoint -> Map Direction [Distance] -> Int -> [Distance]
reachable b bp m = trim distances
  where
    boxDir = boxDirection (0, 0) bp
    distances = map (+ boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir)) (m!boxDir)

reachableCount :: Board -> BoxPoint -> Map Direction [Distance] -> Int -> Int
reachableCount b bp m limit = length $ filter odd r
  where
    r = reachable b bp m limit
-- reachableCount b bp m limit = findIndexS distances limit
--   where
--     boxDir = boxDirection (0, 0) bp
--     distances = map (+ boxMidDistances b (0, 0) (translatedBoxPoint bp boxDir)) (m!boxDir)

-- Fully reachable form a 'diamond shape' of size 2n*(n+1) + 1, where n is the furthest point from middle
fullyReachableBoxes :: Integer -> Integer
fullyReachableBoxes n = 2 * n * (n + 1) + 1

fullyReachableEven :: Bool -> Integer -> Integer
fullyReachableEven oddSteps n = fullyReachableBoxes n - fullyReachableOdd oddSteps n

fullyReachableOdd :: Bool -> Integer -> Integer
fullyReachableOdd oddSteps n
  | oddSteps  = if even n then (n + 1) * (n + 1) else n * n
  | otherwise = if even n then n * n else (n + 1) * (n + 1)

getBorder :: Int -> [Point]
getBorder l = [(0, -l)] ++ topLeft ++ [(-l, 0)] ++ topRight ++ [(0, l)] ++ botRight ++ [(l, 0)] ++ botLeft
  where
    topLeft = [(-r, r - l) | r <- [1..l-1]]
    topRight = [(-r, l - r) | r <- [l-1,l-2..1]]
    botRight = [(r, l - r) | r <- [1..l-1]]
    botLeft = [(r, r - l) | r <- [l-1,l-2..1]]

-- TODO: Could DP be easier here?
main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day21/input.txt"
    let steps = if length args > 1 then ((read :: String -> Int) . last) args else 6
    let board = lines contents
    print $ length $ bfs board steps
    let shortestPathByDir = shortestByDir board
    let test = [((k, True), V.fromList (filter odd v)) | (k, v) <- M.assocs shortestPathByDir]
    let test2 = [((k, False), V.fromList (filter even v)) | (k, v) <- M.assocs shortestPathByDir]
    let vShortestPathByDir = M.fromList (test ++ test2)
    -- let vShortestPathByDir = M.map V.fromList shortestPathByDir
    -- print $ shortest board N
    -- print $ length $ shortest board N
    -- print (shortestPathByDir!N)
    print $ isBoxFullyReachable board (-1, 0) shortestPathByDir steps
    -- let furthestFullyReachable = length $ takeWhile id $ [isBoxFullyReachable board (x, 0) shortestPathByDir steps | x <- [1..]]
    -- Technically this works but not sure if guaranteed
    let furthestFullyReachable = steps `div` length board - 1
    print furthestFullyReachable
    print $ fullyReachableBoxes $ toInteger furthestFullyReachable
    print $ fullyReachableEven (odd steps) $ toInteger furthestFullyReachable
    print $ fullyReachableOdd (odd steps) $ toInteger furthestFullyReachable
    -- By manual inspection, the 'diamond border' of partially reachable boxes is of width 2
    -- I.e. there are some boxes partially reachable that are (furthestFullyReachable + N) away from middle, where N is 1 or 2
    let border1 = getBorder $ furthestFullyReachable + 1
    let border2 = getBorder $ furthestFullyReachable + 2
    let border3 = getBorder $ furthestFullyReachable + 3
    -- This confirms anything closer than the border is fully reachable
    -- let border0 = getBorder $ furthestFullyReachable + 0
    -- print $ all (\bp -> isBoxFullyReachable board bp shortestPathByDir steps) border0
    -- print $ any (\bp -> isBoxFullyReachable board bp shortestPathByDir steps) border0
    print $ all (\bp -> isBoxPartiallyReachable board bp shortestPathByDir steps) border1
    print $ any (\bp -> isBoxPartiallyReachable board bp shortestPathByDir steps) border1
    print $ all (\bp -> isBoxPartiallyReachable board bp shortestPathByDir steps) border2
    print $ any (\bp -> isBoxPartiallyReachable board bp shortestPathByDir steps) border2
    print $ all (\bp -> isBoxPartiallyReachable board bp shortestPathByDir steps) border3
    print $ any (\bp -> isBoxPartiallyReachable board bp shortestPathByDir steps) border3
    print $ reachableCount board (head border1) shortestPathByDir steps
    print $ reachableCount board (last border1) shortestPathByDir steps
    print $ reachableCount board (0, 1) shortestPathByDir steps
    print $ reachableCount board (0, 2) shortestPathByDir steps
    let reachableInOdd = toInteger $ reachableCount board (0, if odd steps then 2 else 1) shortestPathByDir steps
    let reachableInEven = toInteger $ reachableCount board (0, if odd steps then 1 else 2) shortestPathByDir steps
    print reachableInOdd
    print reachableInEven
    let fullyReachableBoxesOdd = fullyReachableOdd (odd steps) $ toInteger furthestFullyReachable
    let fullyReachableBoxesEven = fullyReachableEven (odd steps) $ toInteger furthestFullyReachable
    let totalFullyReachable = reachableInEven * fullyReachableBoxesEven + reachableInOdd * fullyReachableBoxesOdd
    -- This is already too high of an answer, but half of it is too low, so right ballpark
    -- Too low: 568411253849275
    -- Too high: 1168411253849275
    -- Too high: 1168416917032166
    -- Inverted: 1168417030926653
    -- Incorrect: 584211426457260 (Full: 584205649379882)
    -- Incorrect: 584217089640151
    -- Incorrect: 584211412498479
    -- Incorrect: 584211312562858
    print totalFullyReachable
    let reachableBorder = border1 ++ border2
    print $ length reachableBorder
    let vPartiallyReachableCount = toInteger $ sum $ map (\bp -> vReachableCount board bp vShortestPathByDir steps) reachableBorder
    print vPartiallyReachableCount
    let partiallyReachableCount = toInteger $ sum $ map (\bp -> reachableCount board bp shortestPathByDir steps) reachableBorder
    print partiallyReachableCount
    print $ totalFullyReachable + partiallyReachableCount