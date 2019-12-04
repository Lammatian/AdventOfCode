import System.IO
import Debug.Trace
import Utility as U

data Point = Point Int Int deriving (Show)
data Line  = Line Point Point deriving (Show)

sol1 :: String -> Int
sol1 _ = 0

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

crossing :: Line -> Line -> Maybe Point
crossing (Line s1 e1) (Line s2 e2) = Nothing

vertical :: Line -> Bool
vertical (Line (Point x1 _) (Point x2 _)) = x1 == x2

horizontal :: Line -> Bool
horizontal (Line (Point _ y1) (Point _ y2)) = y1 == y2

closest :: [Point] -> Point
closest (p:ps) = closestRec p ps

closestRec :: Point -> [Point] -> Point
closestRec p [] = p
closestRec best (p:ps) =
        if distance p (Point 0 0) < distance best (Point 0 0) then closestRec p ps
        else closestRec best ps

sol2 :: String -> Int
sol2 _ = 0

main :: IO ()
main = do
        handle <- openFile "input0.txt" ReadMode
        contents <- hGetContents handle
        print $ sol1 contents
        print $ sol2 contents