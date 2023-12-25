import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)
import Data.Map (Map, empty, insertWith, keys)
import qualified Data.Map as M (filter, map)

type Point = (Int, Int, Int)
type Velo = (Int, Int, Int)
type Hail = (Point, Velo)
data Var = X | Y | Z
type Remainders = Map Var (Map Int [Int])

parseTuple :: String -> Point
parseTuple t = (a, b, c)
  where
    [a, b, c] = map (read :: String -> Int) $ splitOn ", " t

parseLine :: String -> Hail
parseLine s = (parseTuple a, parseTuple b)
  where
    [a, b] = splitOn " @ " s

getCoeffA :: Hail -> Float
getCoeffA (_, (dx, dy, _)) = dy' / dx'
  where
    dx' = fromIntegral dx
    dy' = fromIntegral dy

getCoeffB :: Hail -> Float
getCoeffB ((x, y, _), (dx, dy, _)) = (y' * dx' - x' * dy') / dx'
  where
    -- Is there a non-warning way to do this?
    [x', y', dx', dy'] = map fromIntegral [x, y, dx, dy]

intersection :: Hail -> Hail -> Maybe (Float, Float)
intersection h1 h2
  | a1 == a2  = Nothing
  | otherwise = Just (x, y)
  where
    a1 = getCoeffA h1
    b1 = getCoeffB h1
    a2 = getCoeffA h2
    b2 = getCoeffB h2
    x = (b2 - b1) / (a1 - a2)
    y = (a1 * b2 - a2 * b1) / (a1 - a2)

withinBounds :: (Float, Float) -> Float -> Float -> Bool
withinBounds (x, y) lb ub = lb <= x && x <= ub && lb <= y && y <= ub

-- Assume no 0s for both dx and dy
isInFuture :: Hail -> (Float, Float) -> Bool
isInFuture ((hx, hy, _), (dx, dy, _)) (x, y)
  | dx < 0 && dy < 0 = hx' > x && hy' > y
  | dx < 0 && dy > 0 = hx' > x && hy' < y
  | dx > 0 && dy < 0 = hx' < x && hy' > y
  | dx > 0 && dy > 0 = hx' < x && hy' < y
  | otherwise        = error "Unexpected input to isInFuture"
  where
    hx' = fromIntegral hx
    hy' = fromIntegral hy

-- Get (x, d_x), (y, d_y) or (z, d_z)
getVar :: Var -> Hail -> (Int, Int)
getVar X ((x, _, _), (dx, _, _)) = (x, dx)
getVar Y ((_, y, _), (_, dy, _)) = (y, dy)
getVar Z ((_, _, z), (_, _, dz)) = (z, dz)

getRemainders :: Var -> [Hail] -> Map Int [Int]
getRemainders _ [] = empty
getRemainders v (h:hs) = insertWith (++) dvh [vh] (getRemainders v hs)
  where
    (vh, dvh) = getVar v h

getDiffs :: [Int] -> [Int]
getDiffs vs = [x - y | x <- vs, y <- vs, x > y]

gcd' :: [Int] -> Int
gcd' vs = foldl gcd (head vs) vs

-- Extended GCD with Bezout
eGCD :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer, Integer)
eGCD (or', 0) (os, _) (ot, _) = (os, ot, or')
eGCD (or', r) (os, s) (ot, t) = eGCD (r, or' - q * r) (s, os - q * s) (t, ot - q * t)
  where
    q = or' `div` r

-- Given equations x = a_i (mod n_i) as form of pairs (a_i, n_i) return solution x = a (mod N) as a pair (a, N)
extendedCRT :: [(Integer, Integer)] -> (Integer, Integer)
extendedCRT [] = error "Empty system of equations"
extendedCRT [(a, m)] = (a `mod` m, m)
extendedCRT ((a, m):(b, n):es) = extendedCRT ((x `mod` md, md):es)
  where
    (u, v, g) = eGCD (m, n) (1, 0) (0, 1)
    md = m * n `div` g
    x = (a * v * n + b * u * m) `div` g

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day24/input.txt"
    let hails = map parseLine $ lines contents
    let lowerBound = if 'e' `elem` head args then 7 else 200000000000000
    let upperBound = if 'e' `elem` head args then 27 else 400000000000000
    let intersections = [(h1, h2, fromJust i) | h1 <- hails, h2 <- hails, h1 /= h2, let i = intersection h1 h2, isJust i]
    let filteredIntersections = filter (\(h1, h2, i) -> withinBounds i lowerBound upperBound && isInFuture h1 i && isInFuture h2 i) intersections
    print $ length filteredIntersections `div` 2
    -- This gets us for each d_v all vs and then calculates the (v_i - v_j) for each pair of vs
    let niceXs = M.filter ((>= 3) . length) $ getRemainders X hails
    let niceYs = M.filter ((>= 3) . length) $ getRemainders Y hails
    let niceZs = M.filter ((>= 3) . length) $ getRemainders Z hails
    -- This gets us the pairs (k, l) such that b_v - k divides l
    -- where b_v is the coefficient of variable v in equation a_v + b_v*v
    let gcdsX = M.map (gcd' . getDiffs) niceXs
    let gcdsY = M.map (gcd' . getDiffs) niceYs
    let gcdsZ = M.map (gcd' . getDiffs) niceZs
    print gcdsX
    print gcdsY
    print gcdsZ
    -- From the above we can figure out what the actual values for b_x, b_y and b_z are for the rock
    let bx = 131 :: Int
    let by = -259 :: Int
    let bz = 102 :: Int
    -- let bx = -3 :: Int
    -- let by = 1 :: Int
    -- let bz = 2 :: Int
    -- Generate equations v = a (mod b_v - d_v) for all coordinates v
    let eqsX = [(toInteger x, toInteger (bx - dx)) | ((x, _, _), (dx, _, _)) <- hails]
    let eqsY = [(toInteger y, toInteger (by - dy)) | ((_, y, _), (_, dy, _)) <- hails]
    let eqsZ = [(toInteger z, toInteger (bz - dz)) | ((_, _, z), (_, _, dz)) <- hails]
    -- Solve these beauties
    print $ extendedCRT eqsX
    print $ extendedCRT eqsY
    print $ extendedCRT eqsZ
    -- Do some maths and get a solution lol