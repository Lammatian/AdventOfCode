import System.Environment (getArgs)
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (ord)
import Data.Map.Ordered as O ((>|), OMap, alter, assocs, empty, member, delete)
import Data.Map as M (adjust, toList, fromList)

type Instruction = String
type Box = OMap String Int

hash :: Instruction -> Int
hash = foldl (\acc c -> (17 * (acc + ord c)) `mod` 256) 0

label :: Instruction -> String
label [] = []
label xs = head $ splitOneOf "=-" xs

flength :: Instruction -> Int
flength [] = -1
flength xs = read $ last $ splitOneOf "=-" xs

boxPower :: (Int, Box) -> Int
boxPower (b, m) = (b + 1) * insidePower 1 (O.assocs m)
  where
    insidePower _ [] = 0
    insidePower i ((_, fl):is) = i * fl + insidePower (i + 1) is

hashmap :: [Instruction] -> Int
hashmap ins = (sum . map boxPower . M.toList) $ go ins (M.fromList [(i, O.empty) | i <- [0..255]])
  where
    go [] m = m
    go (i:is) m = case last i of
      '-' -> go is $ M.adjust (`removeFromBox` label i) (hash (label i)) m
      _   -> go is $ M.adjust (\b -> updateBox b (label i) (flength i)) (hash (label i)) m

removeFromBox :: Box -> String -> Box
removeFromBox m k = O.delete k m

-- TODO: Learn how to lift from (a -> a) to (Maybe a -> Maybe a)
updateBox :: Box -> String -> Int -> Box
updateBox b l i
  | l `O.member` b = O.alter (\_ -> Just i) l b
  | otherwise      = b >| (l, i)

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day15/input.txt"
    let instructions = (splitOn "," . head . lines) contents
    print $ sum $ map hash instructions
    print $ hashmap instructions