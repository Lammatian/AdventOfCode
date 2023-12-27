{-# LANGUAGE TupleSections #-}
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Map (Map, assocs, difference, empty, filterWithKey, fold, fromListWith, insert, insertWith, size, union, unionWith, (!), elems, deleteFindMax, delete)
import qualified Data.Map as M (fromList, map, notMember)
import Data.Set (Set, fromList, toList)
import qualified Data.Set as S (map, foldl, member)
import Data.Tuple (swap)
import Debug.Trace (trace)

type Graph = Map Edge Int
type Graph' = Map Node (Map Node Int)
type Edge = (Node, Node)
type Node = String

parseLine :: String -> [(String, String)]
parseLine s = toList $ fromList $ [(left, r) | r <- rights] ++ [(r, left) | r <- rights]
  where
    [left, right] = splitOn ": " s
    rights = splitOn " " right

toEdge :: Node -> Node -> Edge
toEdge a b
  | a < b = (a, b)
  | otherwise = (b, a)

parseGraph :: [String] -> Graph
parseGraph ss = go edges
  where
    edges = concatMap parseLine ss
    go [] = empty
    go ((k, v) : es) = insert (toEdge k v) 1 $ go es

weight :: Graph -> Node -> Node -> Int
weight g a b = g ! toEdge a b

edges :: Graph -> Node -> Graph
edges g n = filterWithKey (\(a, b) _ -> a == n || b == n) g

edgesFrom :: Graph -> [Node] -> Graph
edgesFrom g ns = foldl union empty $ map (edges g) ns

-- Remove old edges
-- Add new edges as sum of the connections
mergeNodes :: Graph -> Node -> Node -> Graph
mergeNodes g a b = (g `difference` oldEdges) `union` newEdges
  where
    aEdges = edges g a
    bEdges = edges g b
    -- This is sad
    mergeA = M.fromList $ map (\((ka, kb), v) -> (toEdge (a ++ b) (if a == ka then kb else ka), v)) $ filter (\(e, _) -> e /= toEdge a b) $ assocs aEdges
    mergeB = M.fromList $ map (\((ka, kb), v) -> (toEdge (a ++ b) (if b == ka then kb else ka), v)) $ filter (\(e, _) -> e /= toEdge a b) $ assocs bEdges
    oldEdges = aEdges `union` bEdges
    newEdges = unionWith (+) mergeA mergeB

-- TODO: store the neighbours as a prio queue or something to speed it up
findTightlyConnected :: Graph -> Node -> Node
findTightlyConnected g n = if a == n then b else a
  where
    (a, b) = fst $ foldl (\(e, v) (e2, v2) -> if v2 > v then (e2, v2) else (e, v)) (("", ""), 0) $ assocs $ edges g n

-- mergeNodes2 :: Graph' -> Node -> Node -> Graph'
-- mergeNodes2 g a b = (g `difference` oldEdges) `union` newEdges
--   where
--     aEdges = edges g a
--     bEdges = edges g b
--     -- This is sad
--     mergeA = M.fromList $ map (\((ka, kb), v) -> (toEdge (a ++ b) (if a == ka then kb else ka), v)) $ filter (\(e, _) -> e /= toEdge a b) $ assocs aEdges
--     mergeB = M.fromList $ map (\((ka, kb), v) -> (toEdge (a ++ b) (if b == ka then kb else ka), v)) $ filter (\(e, _) -> e /= toEdge a b) $ assocs bEdges
--     oldEdges = aEdges `union` bEdges
--     newEdges = unionWith (+) mergeA mergeB

-- findMinCut2 :: Graph' -> Map Node Int -> Node -> ([Node], Node, Node, Int)
-- findMinCut2 g a t
--   | size g == 1 = (map fst (assocs a), t, newT, cutVal)
--   | otherwise   = findMinCut2 g updatedA newT
--   where
--     (cutVal, newT) = maximum $ map swap $ assocs a
--     newA = delete newT a
--     edgesT = filterWithKey (\k _ -> k `M.notMember` newA) (g!newT)
--     updatedA = unionWith (+) newA edgesT

-- minCutPhase2 :: Graph' -> Node -> (Graph', (Int, Node, Node))
-- minCutPhase2 g n = (mergeNodes g s t, (v, concat a, t))
--   where
--     (a, s, t, v) = findMinCut2 g [n] ""

findMinCut :: Graph -> Node -> Node -> (Node, Node, Node, Int)
findMinCut g a t
  | size g == 1 = (a, t, newT, cutVal)
  | otherwise   = findMinCut (mergeNodes g a newT) (a ++ newT) newT
  where
    newT = findTightlyConnected g a
    cutVal = head $ elems g

minCutPhase :: Graph -> Node -> (Graph, (Int, Node, Node))
minCutPhase g n = (mergeNodes g s t, (v, a, t))
  where
    (a, s, t, v) = findMinCut g n ""

-- TODO: I'd prefer 'Node' parameter not to be there, but for testing it's better
minCut :: Graph -> Node -> (Int, Node, Node)
minCut g n
  | size g <= 2 = snd $ minCutPhase g n
  | otherwise   = min v $ minCut (trace (show (size newG)) newG) n
  where
    (newG, v) = minCutPhase g n

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day25/input.txt"
    let graph = parseGraph $ lines contents
    -- let graph = M.fromList [(("1", "2"), 2)
    --                        , (("1", "5"), 3)
    --                        , (("2", "3"), 3)
    --                        , (("2", "5"), 2)
    --                        , (("2", "6"), 2)
    --                        , (("3", "4"), 4)
    --                        , (("3", "7"), 2)
    --                        , (("4", "7"), 2)
    --                        , (("4", "8"), 2)
    --                        , (("5", "6"), 3)
    --                        , (("6", "7"), 1)
    --                        , (("7", "8"), 3)]
    -- print graph
    -- print $ mergeNodes graph "1" "5"
    -- print $ findTightlyConnected graph "2"
    -- print $ findMinCut graph "2" ""
    -- print $ minCutPhase graph "2"
    -- print $ minCut graph "2"
    print $ minCut graph "jqt"