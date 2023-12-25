import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

type Point = (Int, Int, Int)
type Velo = (Int, Int, Int)
type Hail = (Point, Velo)

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