{-# LANGUAGE TupleSections #-}
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Map (Map, assocs, empty, filterWithKey, insert, size, unionWith, (!), delete, keys, findWithDefault)
import qualified Data.Map as M (filter, null)
import Data.Tuple (swap)
import Debug.Trace (trace)

type Graph' = Map Node (Map Node Int)
type Edge = (Node, Node)
type Node = String

parseLine :: String -> [(String, String)]
parseLine s = [(left, r) | r <- rights] ++ [(r, left) | r <- rights]
  where
    [left, right] = splitOn ": " s
    rights = splitOn " " right

parseGraph :: [String] -> Graph'
parseGraph [] = empty
parseGraph (s:ss) = foldl (\g e -> addEdge g e 1) (parseGraph ss) ns
  where
    ns = parseLine s

removeNode :: Graph' -> Node -> Graph'
removeNode g n = newG
  where
    oldEdges = keys $ g!n
    newG = M.filter (not . M.null) $ foldl (\g' e -> insert e (delete n (g'!e)) g') (delete n g) oldEdges

addEdge :: Graph' -> Edge -> Int -> Graph'
addEdge g (a, b) v = insertedToBoth
  where
    insertedToA = insert a (insert b v (findWithDefault empty a g)) g
    insertedToBoth = insert b (insert a v (findWithDefault empty b insertedToA)) insertedToA

addEdges :: Graph' -> [(Edge, Int)] -> Graph'
addEdges g [] = g
addEdges g ((e, v):es) = addEdges (addEdge g e v) es

mergeNodes :: Graph' -> Node -> Node -> Graph'
mergeNodes g a b = newG
  where
    -- TODO: Method to join edge names, for nicer types
    newEdges = map (\(n, v) -> ((a ++ b, n), v)) $ assocs $ unionWith (+) (delete b (g!a)) (delete a (g!b))
    removedA = g `removeNode` a
    removedBoth = removedA `removeNode` b
    newG = addEdges removedBoth newEdges

findMinCut :: Graph' -> [Node] -> Map Node Int -> Node -> ([Node], Node, Node, Int)
findMinCut g ns a t
  | length ns == size g - 1 = (ns, t, newT, cutVal)
  | otherwise   = findMinCut g (newT:ns) updatedA newT
  where
    (cutVal, newT) = maximum $ map swap $ assocs a
    newA = delete newT a
    edgesT = filterWithKey (\k _ -> k `notElem` ns) (g!newT)
    updatedA = unionWith (+) newA edgesT

minCutPhase :: Graph' -> Node -> (Graph', (Int, Node, Node))
minCutPhase g n = (mergeNodes g s t, (v, concat a, t))
  where
    (a, s, t, v) = findMinCut g [n] (g!n) ""

minCut :: Graph' -> Node -> (Int, Node, Node)
minCut g n
  | size g <= 2 = snd $ minCutPhase g n
  | otherwise   = min v $ minCut (trace (show (size newG)) newG) n
  where
    (newG, v) = minCutPhase g n

-- IDEA: Randomised picking of (source, target) and counting edges along the way
--       The most picked edges over many iterations will be the cuts
main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day25/input.txt"
    -- Test input from https://e-maxx.ru/bookz/files/stoer_wagner_mincut.pdf
    -- let graph = M.fromList [("1", M.fromList [("2", 2), ("5", 3)])
    --                        ,("2", M.fromList [("1", 2), ("3", 3), ("5", 2), ("6", 2)])
    --                        ,("3", M.fromList [("2", 3), ("4", 4), ("7", 2)])
    --                        ,("4", M.fromList [("3", 4), ("7", 2), ("8", 2)])
    --                        ,("5", M.fromList [("1", 3), ("2", 2), ("6", 3)])
    --                        ,("6", M.fromList [("2", 2), ("5", 3), ("7", 1)])
    --                        ,("7", M.fromList [("3", 2), ("4", 2), ("6", 1), ("8", 3)])
    --                        ,("8", M.fromList [("4", 2), ("7", 3)])
    --                        ]
    let graph = parseGraph $ lines contents
    let (_, l, r) = minCut graph "jqt"
    print $ (length l `div` 3) * (length r `div` 3)