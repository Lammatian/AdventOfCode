import System.Environment (getArgs)
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (toUpper)
import Data.Map (Map, fromList, (!), insert)

data Category = X | M | A | S deriving (Show, Read, Eq, Ord)
type WfName = String
data Rule = Rule Category Ordering Int WfName | Otherwise WfName deriving (Show)
data Workflow = Workflow {
  unName :: WfName,
  unRule :: [Rule]
} deriving (Show)
data Part = Part {
  unX :: Int,
  unM :: Int,
  unA :: Int,
  unS :: Int
} deriving (Show)
data Range = Range Int Int | Empty deriving (Show)
type XmasRange = Map Category Range

parseOrderRule :: String -> Rule
parseOrderRule s
  | '<' `elem` s = Rule cat LT val wfname
  | otherwise    = Rule cat GT val wfname
  where
    [catStr, rest] = splitOneOf "<>" s
    cat = read $ map toUpper catStr
    [valStr, wfname] = splitOn ":" rest
    val = read valStr

parseRule :: String -> Rule
parseRule s
  | ':' `elem` s = parseOrderRule s
  | otherwise    = Otherwise s

parseRules :: String -> [Rule]
parseRules s = map parseRule $ splitOn "," s

parseWorkflow :: String -> Workflow
parseWorkflow s = Workflow name (parseRules ruleStr)
  where
    [name, ruleStr] = splitOn "{" $ init s

toPart :: [Int] -> Part
toPart xs
  | length xs /= 4 = error "Unexpected part categorisation"
  | otherwise      = Part (head xs) (xs!!1) (xs!!2) (xs!!3)

parsePart :: String -> Part
parsePart s = toPart $ map ((read :: String -> Int) . last . splitOn "=") $ splitOn "," $ init $ tail s

ruleTarget :: Rule -> WfName
ruleTarget (Otherwise w) = w
ruleTarget (Rule _ _ _ w) = w

getCategory :: Part -> Category -> Int
getCategory p X = unX p
getCategory p M = unM p
getCategory p A = unA p
getCategory p S = unS p

satisfies :: Part -> Rule -> Maybe WfName
satisfies _ (Otherwise n) = Just n
satisfies p (Rule cat ord v n)
  | compare (getCategory p cat) v == ord = Just n
  | otherwise                            = Nothing

apply :: Part -> Workflow -> WfName
apply part (Workflow _ rules) = go part rules
  where
    go _ [] = error "Part did not satisfy any condition"
    go p (r:rs) = case satisfies p r of
      Just wfname -> wfname
      Nothing     -> go p rs

isAccepted :: WfName -> Bool
isAccepted "A" = True
isAccepted _   = False

processRec :: Part -> WfName -> Map WfName Workflow -> WfName
processRec p n m
  | n `elem` ["A", "R"] = n
  | otherwise           = processRec p (apply p (m ! n)) m

process :: Part -> Map WfName Workflow -> WfName
process p = processRec p "in"

partRating :: Part -> Int
partRating (Part x m a s) = x + m + a + s

validate :: Range -> Range
validate Empty = Empty
validate r@(Range l u) = if l <= u then r else Empty

(<->) :: Range -> Rule -> Range
(<->) Empty _ = Empty
(<->) _ (Otherwise _) = error "Should not call difference between Otherwise rule and range"
(<->) (Range l u) (Rule _ LT v _) = validate $ Range (max l v) u
(<->) (Range l u) (Rule _ GT v _) = validate $ Range l (min v u)
(<->) _ _ = error "Unsupported difference parameters"

(<^>) :: Range -> Rule -> Range
(<^>) Empty _ = Empty
(<^>) xr (Otherwise _) = xr
(<^>) (Range l u) (Rule _ LT v _) = validate $ Range l (min u (v - 1))
(<^>) (Range l u) (Rule _ GT v _) = validate $ Range (max l (v + 1)) u
(<^>) _ _ = error "Unsupported intersection parameters"

fullRange :: Range
fullRange = Range 1 4000

fullXmasRange :: XmasRange
fullXmasRange = let f = fullRange in fromList [(c, f) | c <- [X, M, A, S]]

difference :: XmasRange -> Rule -> XmasRange
difference xr r@(Rule cat _ _ _) = insert cat ((xr!cat) <-> r) xr
difference _ _ = error "Should not call difference between Otherwise rule and range"

intersection :: XmasRange -> Rule -> XmasRange
intersection xr r@(Rule cat _ _ _) = insert cat ((xr!cat) <^> r) xr
intersection xr _ = xr

rangeSize :: Range -> Int
rangeSize (Range l u) = u - l + 1
rangeSize Empty = 0

xmasRangeSize :: XmasRange -> Int
xmasRangeSize xr = product [rangeSize (xr!c) | c <- [X, M, A, S]]

processRangeRec :: XmasRange -> WfName -> Map WfName Workflow -> [XmasRange]
processRangeRec _ "R" _ = []
processRangeRec r "A" _ = [r]
processRangeRec r n m = go r (unRule (m!n))
  where
    go _ [] = []
    go r (wr:wrs) = processRangeRec (r `intersection` wr) (ruleTarget wr) m ++ go (r `difference` wr) wrs

processRange :: XmasRange -> Map WfName Workflow -> [XmasRange]
processRange x = processRangeRec x "in"

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day19/input.txt"
    let [workflowsStr, partsStr] = map lines $ splitOn "\n\n" contents
    let workflowList = map parseWorkflow workflowsStr
    let workflows = fromList $ map (\w -> (unName w, w)) workflowList
    let parts = map parsePart partsStr
    let processed = map (\p -> (p, process p workflows)) parts
    print $ sum $ [partRating p | (p, a) <- processed, isAccepted a]
    print $ sum $ map xmasRangeSize $ processRange fullXmasRange workflows