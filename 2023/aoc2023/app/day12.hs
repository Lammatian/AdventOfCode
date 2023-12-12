import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Map as Map (Map, lookup, insert, empty)
import Control.Monad.State.Lazy (State, evalState, get, put)
-- TODO: Use MemoTrie or some other memoization library instead of custom code
--       This way I would be able to use `arrangements` instead of modifying it

type Pattern = String
type Groups = [Int]
data Record = R {
  pt :: Pattern,
  gr :: Groups
} deriving (Show, Eq, Ord)

parseLine :: String -> Record
parseLine s = R x (map read (splitOn "," y))
  where
    [x, y] = words s

fit :: Pattern -> Int -> Maybe Pattern
fit [] _ = Nothing
fit s g
  | length s < g  = Nothing
  | length s == g = if allSpotsFit then Just "" else Nothing
  | s !! g == '#' = Nothing
  | allSpotsFit   = Just (drop (g + 1) s)
  | otherwise     = Nothing
  where
    allSpotsFit = all (`elem` "#?") (take g s)

arrangements :: Record -> Int
arrangements (R [] []) = 1
arrangements (R [] _) = 0
arrangements (R (p:ps) [])
  | p `elem` ['.', '?'] = arrangements (R ps [])
  | otherwise           = 0
arrangements (R ('.':ps) gs) = arrangements (R ps gs)
arrangements (R ('?':ps) (g:gs)) = case fit ('?':ps) g of
  Just ps2 -> arrangements (R ps2 gs) + arrangements (R ps (g:gs))
  Nothing  -> arrangements (R ps (g:gs))
arrangements (R ('#':ps) (g:gs)) = case fit ('#':ps) g of
  Just ps2 -> arrangements (R ps2 gs)
  Nothing  -> 0
arrangements (R _ _) = error "Unmatched pattern"

expandRecord :: Record -> Record
expandRecord (R ps gs) = R (unfold ps) (concat (replicate 5 gs))
  where
    unfold = intercalate "?" . replicate 5

-- Taken from https://monospacedmonologues.com/2022/01/memotries/, but modified to fix a B U G
memoize :: Ord a => (a -> State (Map a b) b) -> a -> State (Map a b) b
memoize f x = do
  computed <- get
  case Map.lookup x computed of
    Just result -> do
      return result
    Nothing -> do
      result <- f x
      -- Here was the bug: the map gets updated in recursion above, cannot use 'computed' anymore
      updatedMap <- get
      put $ Map.insert x result updatedMap
      return result

arrangements' :: Record -> State (Map Record Int) Int
arrangements' (R [] []) = return 1
arrangements' (R [] _) = return 0
arrangements' (R (p:ps) [])
  | p `elem` ".?" = do memoisedArrangements (R ps [])
  | otherwise     = return 0
arrangements' (R ('.':ps) gs) = do memoisedArrangements (R ps gs)
arrangements' (R ('?':ps) (g:gs)) =
  case fit ('?':ps) g of
    Just ps2 ->
      do
        a <- memoisedArrangements (R ps2 gs)
        b <- memoisedArrangements (R ps (g:gs))
        return $ a + b
    Nothing  -> do memoisedArrangements (R ps (g:gs))
arrangements' (R ('#':ps) (g:gs)) =
  case fit ('#':ps) g of
    Just ps2 -> do memoisedArrangements (R ps2 gs)
    Nothing  -> return 0
arrangements' (R _ _) = error "Unmatched pattern"

-- I wish I understood this
-- It uses some Haskell's lazy magic + naming a function so that it doesn't go out of scope or sth
memoisedArrangements :: Record -> State (Map Record Int) Int
memoisedArrangements = memoize arrangements'

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day12/input.txt"
    let records = map parseLine $ lines contents
    print $ sum $ map arrangements records
    let expandedRecords = map expandRecord records
    print $ sum $ map (\r -> evalState (arrangements' r) Map.empty) expandedRecords
