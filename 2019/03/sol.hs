module Main where

import System.Environment
import System.IO
import Debug.Trace
import Data.Maybe

import Utility as U

data Point    = Point Int Int            deriving (Show)
data Line     = Line Point Point         deriving (Show)
data DistLine = DistLine Point Point Int deriving (Show)

instance Eq Point where
    (==) (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2

central :: Point
central = Point 0 0

getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y

getStart :: Line -> Point
getStart (Line s _) = s

getEnd :: Line -> Point
getEnd (Line _ e) = e

getStartX :: Line -> Int
getStartX = getX . getStart

getStartY :: Line -> Int
getStartY = getY . getStart

getEndX :: Line -> Int
getEndX = getX . getEnd

getEndY :: Line -> Int
getEndY = getY . getEnd

lineLength :: Line -> Int
lineLength l = abs (getStartX l - getEndX l) + abs (getStartY l - getEndY l)

addX :: Point -> Int -> Point
addX (Point x y) i = Point (x + i) y

addY :: Point -> Int -> Point
addY (Point x y) i = Point x (y + i)

distance :: Point -> Point -> Int
distance p1 p2 = abs (getX p1 - getX p2) + abs (getY p1 - getY p2)

vertical :: Line -> Bool
vertical l = getStartX l == getEndX l

horizontal :: Line -> Bool
horizontal = not . vertical

toPointList :: Line -> [Point]
toPointList l = 
        if horizontal l then [ Point x (getStartY l) | x <- [ min (getStartX l) (getEndX l) .. max (getStartX l) (getEndX l) ] ]
        else [ Point (getStartX l) y | y <- [ min (getStartY l) (getEndY l) .. max (getStartY l) (getEndY l) ] ]

common :: Line -> Line -> [Point]
common l1@(Line p1 p2) l2@(Line p3 p4)
        | horizontal s1 && horizontal s2 && getStartY s1 == getStartY s2 = [ Point x (getStartY s1) | x <- [ max (getStartX s1) (getStartX s2) .. min (getEndX s1) (getEndX s2) ] ]
        | vertical s1 && vertical s2 && getStartX s1 == getStartX s2     = [ Point (getStartX s1) y | y <- [ max (getStartY s1) (getStartY s2) .. min (getEndY s1) (getEndY s2) ] ]
        | horizontal s1 && vertical s2 =
            [ Point (getStartX s2) (getStartY s1) | getStartX s1 < getStartX s2 && getStartX s2 < getEndX s1 && getStartY s2 < getStartY s1 && getStartY s1 < getEndY s2 ]
            --if getStartX s1 < getStartX s2 && getStartX s2 < getEndX s1 && getStartY s2 < getStartY s1 && getStartY s1 < getEndY s2 then [Point (getStartX s2) (getStartY s1)] else []
        | otherwise = 
            [ Point (getStartX s1) (getStartY s2) | getStartX s2 < getStartX s1 && getStartX s1 < getEndX s2 && getStartY s1 < getStartY s2 && getStartY s2 < getEndY s1 ]
            --if getStartX s2 < getStartX s1 && getStartX s1 < getEndX s2 && getStartY s1 < getStartY s2 && getStartY s2 < getEndY s1 then [Point (getStartX s1) (getStartY s2)] else []
    where s1 = if horizontal l1 then Line (smallerByX p1 p2) (biggerByX p1 p2) else Line (smallerByY p1 p2) (biggerByY p1 p2)
          s2 = if horizontal l2 then Line (smallerByX p3 p4) (biggerByX p3 p4) else Line (smallerByY p3 p4) (biggerByY p3 p4)
          smallerByX p1 p2 = if getX p1 < getX p2 then p1 else p2
          biggerByX p1 p2  = if getX p1 > getX p2 then p1 else p2
          smallerByY p1 p2 = if getY p1 < getY p2 then p1 else p2
          biggerByY p1 p2  = if getY p1 > getY p2 then p1 else p2

closest :: [Point] -> Point -> Maybe Point
closest [] p     = Nothing
closest (f:ps) p = Just $ foldr (\x acc -> if distance x p < distance acc p then x else acc) f ps

getClosestCrossing :: Line -> Line -> Maybe Point -- Returns closest crossing point of 2 lines if such exists
getClosestCrossing l1 l2 = closest (common l1 l2) central

moves2points :: [String] -> Point -> [Point]
moves2points [] curr     = [curr]
moves2points (m:ms) curr = case head m of
                                'R' -> curr : moves2points ms (addX curr value)
                                'L' -> curr : moves2points ms (addX curr (-value))
                                'U' -> curr : moves2points ms (addY curr value)
                                'D' -> curr : moves2points ms (addY curr (-value))
                                _   -> []
                                where value = U.readInt $ tail m

points2lines :: [Point] -> [Line]
points2lines (x:y:xs) = Line x y : points2lines (y:xs)
points2lines _        = []

closerToCenter :: Maybe Point -> Maybe Point -> Maybe Point
closerToCenter Nothing Nothing     = Nothing
closerToCenter (Just p) Nothing    = if p == central then Nothing else Just p
closerToCenter Nothing (Just p)    = if p == central then Nothing else Just p
closerToCenter (Just p1) (Just p2) | p1 == central = closerToCenter Nothing (Just p2)
                                   | p2 == central = closerToCenter (Just p1) Nothing
                                   | otherwise     = closest [p1, p2] central 

closestOfLines :: Line -> [Line] -> Maybe Point
closestOfLines l = foldr (closerToCenter . getClosestCrossing l) Nothing

closestOfPoints :: [Maybe Point] -> Maybe Point
closestOfPoints = foldr closerToCenter Nothing

sol1 :: String -> Int
sol1 s = case closestOfPoints $ map (\l -> closestOfLines l (lines!!1)) (head lines) of
            Just p  -> distance p central
            Nothing -> -1
        where input  = U.splitLines s
              moves  = map (U.splitOn ',') input
              points = map (`moves2points` central) moves
              lines  = map points2lines points

firstCrossingTime :: (Int, Line) -> [(Int, Line)] -> Maybe Int
firstCrossingTime (d1, l1) []    = Nothing
firstCrossingTime (d1, l1) ((d2, l2):ls) = if not (null (common l1 l2)) && d1 + d2 > 0
                                           then Just (d1 + d2 + distance (getStart l1) fstCommon + distance (getStart l2) fstCommon)
                                           else firstCrossingTime (d1, l1) ls
                                           where fstCommon = head (common l1 l2)

toDistLines :: [Line] -> [(Int, Line)]
toDistLines ls = toDistLinesRec ls 0

toDistLinesRec :: [Line] -> Int -> [(Int, Line)]
toDistLinesRec [] _        = []
toDistLinesRec (l:ls) dist = (dist, l) : toDistLinesRec ls (dist + lineLength l)

sol2 :: String -> Int
sol2 s = minimum $ mapMaybe (\x -> firstCrossingTime x (dlines!!1)) (head dlines)
        where input  = U.splitLines s
              moves  = map (U.splitOn ',') input
              points = map (`moves2points` central) moves
              lines  = map points2lines points
              dlines = map toDistLines lines

main = do
    [filename] <- getArgs
    handle     <- openFile filename ReadMode
    contents   <- hGetContents handle
    print $ sol1 contents
    print $ sol2 contents
