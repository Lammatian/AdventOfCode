import System.Environment (getArgs)
import Data.List.Split (splitOn)

type Point = (Int, Int)
data Direction = N | E | S | W deriving (Show)
data Instruction = I Direction Int deriving (Show)

parseDirection :: String -> Direction
parseDirection "U" = N
parseDirection "R" = E
parseDirection "D" = S
parseDirection "L" = W
parseDirection _   = error "Unrecognised direction"

parseInstruction :: String -> Instruction
parseInstruction s = I (parseDirection d) (read c)
  where
    [d, c, _] = splitOn " " s

parseHexDirection :: Char -> Direction
parseHexDirection '0' = E
parseHexDirection '1' = S
parseHexDirection '2' = W
parseHexDirection '3' = N
parseHexDirection _   = error "Unrecognised hex direction"

parseHexDistance :: String -> Int
parseHexDistance x = (read :: String -> Int) $ "0x" ++ x

parseHexInstruction :: String -> Instruction
parseHexInstruction s = I (parseHexDirection dirStr) (parseHexDistance distStr)
  where
    [_, _, c] = splitOn " " s
    hexnum = init $ drop 2 c
    dirStr = last hexnum
    distStr = init hexnum

move :: Point -> Instruction -> Point
move (r, c) (I N m) = (r - m, c)
move (r, c) (I E m) = (r, c + m)
move (r, c) (I S m) = (r + m, c)
move (r, c) (I W m) = (r, c - m)

distance :: Instruction -> Int
distance (I _ d) = d

followRec :: [Instruction] -> [Point] -> Int -> ([Point], Int)
followRec _ [] _ = error "This method should not be called with empty point list"
followRec [] ps c = (ps, c)
followRec (i:is) (p:ps) c = followRec is newPs newC
  where
    newP = move p i
    newPs = newP : p : ps
    newC = c + distance i

-- Given instructions, returns the vertices of the polygon and the border length
follow :: [Instruction] -> ([Point], Int)
follow is = followRec is [(0, 0)] 0

-- Let's apply learnings from day 10: Shoelace formula and Pick's theorem 🤓
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea [_] = 0
doubleArea ((r1, c1):p2@(r2, c2):ps) = (r1 + r2) * (c1 - c2) + doubleArea (p2:ps)

area :: [Point] -> Int
area ps = abs $ doubleArea (last ps:ps) `div` 2

-- Pick's theorem: Area = #interiorPts + #borderPts/2 - 1
pick :: [Point] -> Int -> Int
pick ps b = i + b
  where
    -- Let's just assume border length is always even lol
    i = area ps - (b `div` 2) + 1

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day18/input.txt"
    let ls = lines contents
    let instructions = map parseInstruction ls
    let (border, borderLength) = follow instructions
    print $ pick border borderLength
    let hexInstructions = map parseHexInstruction ls
    let (hexBorder, hexBorderLength) = follow hexInstructions
    print $ pick hexBorder hexBorderLength